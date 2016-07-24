{-# LANGUAGE FlexibleContexts #-}

module EntangleMonad where

import Control.Monad
import Data.Foldable hiding (foldr, concatMap)
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as DM
import Data.Maybe
import Data.Monoid
import Debug.Trace

import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer

type BitState = Map BitId Bool

data EntangleMonad a = EntangleMonad {
    untangle :: BitState -> CircTree (BitState, a)
}

instance Monad EntangleMonad where
    return x = EntangleMonad (\bs -> LeafNode (bs, x))
    x >>= f = EntangleMonad $ \bs -> do
        (bs', y) <- untangle x bs
        untangle (f y) bs'

data CircTree a
    = GateNode String [QubitId] [B_Endpoint QubitId BitId] (CircTree a)
    | MeasureNode QubitId BitId (CircTree a) (CircTree a)
    | LeafNode a

instance Functor CircTree where
    fmap = liftM

instance Monad CircTree where
    return = LeafNode
    (GateNode n qs cs t) >>= f = GateNode n qs cs (t >>= f)
    (MeasureNode q b l r) >>= f = MeasureNode q b (l >>= f) (r >>= f)
    (LeafNode x) >>= f = f x

instance Foldable CircTree where
    foldMap f (GateNode _ _ _ t) = foldMap f t
    foldMap f (MeasureNode _ _ l r) = foldMap f l <> foldMap f r
    foldMap f (LeafNode x) = f x

instance Show a => Show (CircTree a) where
    show = showTree 0 show' child where
        child (GateNode _ _ _ c) = [c]
        child (MeasureNode _ _ l r) = [l, r]
        child (LeafNode _) = []
        show' (GateNode n qs cs _) = "GateNode \"" ++ n ++ "\" on qubits " ++ qubits ++ (if null controls then "" else " and controls " ++ controls) where
            qubits   = intercalate ", " $ map (show . unqubit) qs
            controls = intercalate ", " $ map showe cs
            showe (Endpoint_Qubit i) = "Q" ++ show (unqubit i)
            showe (Endpoint_Bit i) = "B" ++ show (unbit i)
        show' (MeasureNode q b _ _) = "MeasureNode on qubit " ++ show (unqubit q) ++ " producing bit " ++ show (unbit b)
        show' (LeafNode x) = "LeafNode of " ++ show x

showTree :: Int -> (a -> String) -> (a -> [a]) -> a -> String
showTree i sf cf t = indent i ++ sf t ++ concatMap (\c -> "\n" ++ showTree (i+1) sf cf c) (cf t)

indent :: Int -> String
indent i = replicate (4*i) ' '

transformGate :: String -> [QubitId] -> [B_Endpoint QubitId BitId] -> EntangleMonad ()
transformGate name qs cs = EntangleMonad res where
    res bs = GateNode name qs cs $ LeafNode (bs, ())

transformMeasure :: QubitId -> EntangleMonad BitId
transformMeasure i = EntangleMonad res where
    res bs = MeasureNode i new l r where
        lastBit = foldr (max . unbit) 0 $ DM.keys bs
        new = BitId $ 1 + lastBit

        bsF = DM.insert new False bs
        l = LeafNode (bsF, new)

        bsT = DM.insert new True bs
        r = LeafNode (bsT, new)

transformDynamicLifting :: BitId -> EntangleMonad Bool
transformDynamicLifting i = EntangleMonad res where
    res bs = LeafNode (bs, b) where
        b = fromMaybe False $ DM.lookup i bs

newtype QubitId = QubitId { unqubit :: Int }
newtype BitId = BitId { unbit :: Int } deriving (Eq, Ord)

-- |mytransformer is the main transformer.
-- it used to extract the needed information from a Quipper circuit.
mytransformer :: Transformer EntangleMonad QubitId BitId
mytransformer g | trace (show g) False = undefined
mytransformer (T_QGate name _ _ _ _ f) = f g where
    open (Signed _ False) = error "Negative controls are not supported yet"
    open (Signed x True) = x
    g wires g_controls controls = do
        transformGate name wires (map open controls)
        return (wires, g_controls, controls)
mytransformer (T_QMeas f) = f transformMeasure
mytransformer (T_DTerm _ f) = f (const $ return ())
mytransformer g = error $ "Gate \"" ++ show g ++ "\" is not supported yet"

mydtransformer :: DynamicTransformer EntangleMonad QubitId BitId
mydtransformer = DT mytransformer (error "boxed circuits missing") transformDynamicLifting

instance Show a => Show (ReadWrite a) where
    show (RW_Return a) = "RW_Return " ++ show a
    show (RW_Write gate next) = "RW_Write " ++ show gate ++ "\n" ++ show next
    show (RW_Read wire f) = "RW_Read " ++ show wire ++ "\n" ++ show (f True)
    show (RW_Subroutine boxId typedSubroutine next) = "RW_Subroutine " ++ show boxId ++ "\n" ++ show next

showIndented :: Show a => Int -> a -> String
showIndented i x = indent ++ replace (show x) where
    indent = replicate (i*4) ' '
    replace [] = []
    replace ('\n':ss) = '\n' : indent ++ replace ss
    replace (s:ss) = s : replace ss

-- |buildTree takes a 'Circuit', its arity and returns a tree representing it.
--buildTree :: DBCircuit x -> Int -> CircTree x
buildTree :: Show x => DBCircuit x -> Int -> CircTree x
buildTree circuit n | trace ("n:\n    " ++ show n ++ "\ncircuit:\n" ++ showIndented 1 circuit) False = undefined
buildTree circuit n = fmap (fst . snd) res where
    res = untangle monad DM.empty
    monad = transform_dbcircuit mydtransformer circuit bindings
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) (QubitId i)) bindings_empty [1..n]
