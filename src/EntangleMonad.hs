{-# LANGUAGE FlexibleContexts #-}

module EntangleMonad where

import Control.Monad
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as DM
import Data.Maybe
import Data.Monoid
--import Debug.Trace

import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer

newtype QubitId = QubitId { unqubit :: Int } deriving Eq
newtype BitId = BitId { unbit :: Int } deriving (Eq, Ord)

type BitState = Map BitId Bool

newtype EntangleMonad a = EntangleMonad {
    untangle :: BitState -- ^ state of bits before the operation
             -> [QubitId] -- ^ measured qubits before the operation
             -> CircTree (BitState, [QubitId], a) -- ^ tree after the operation
}

instance Functor EntangleMonad where
    fmap = liftM

instance Applicative EntangleMonad where
    pure = return
    (<*>) = ap

instance Monad EntangleMonad where
    return x = EntangleMonad (\bs ms -> LeafNode (bs, ms, x))
    x >>= f = EntangleMonad $ \bs ms -> do
        (bs', ms', y) <- untangle x bs ms
        untangle (f y) bs' ms'

data CircTree a
    = GateNode String [QubitId] [QubitId] (CircTree a) -- ^ name, affected qubits, controls, child
    | MeasureNode QubitId BitId (CircTree a) (CircTree a)
    | LeafNode a

instance Functor CircTree where
    fmap = liftM

instance Applicative CircTree where
    pure = return
    (<*>) = ap

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
            controls = intercalate ", " $ map (show . unqubit) cs
        show' (MeasureNode q b _ _) = "MeasureNode on qubit " ++ show (unqubit q) ++ " producing bit " ++ show (unbit b)
        show' (LeafNode x) = "LeafNode of " ++ show x

showTree :: Int -> (a -> String) -> (a -> [a]) -> a -> String
showTree i sf cf t = indent i ++ sf t ++ concatMap showChild children where
    children = cf t
    fork = length children > 1
    showChild c = "\n" ++ showTree (if fork then i+1 else i) sf cf c

indent :: Int -> String
indent i = replicate (4*i) ' '

transformGate :: String -> [QubitId] -> [QubitId] -> EntangleMonad ()
transformGate name qs cs = EntangleMonad res where
    res bs ms = GateNode name qs cs $ LeafNode (bs, ms, ())

transformMeasure :: QubitId -> EntangleMonad BitId
--transformMeasure i | trace ("Measuring " ++ show (unqubit i)) False = undefined
transformMeasure i = EntangleMonad res where
    res bs ms = MeasureNode i new l r where
        ms' = i : ms

        lastBit = foldr (max . unbit) 0 $ DM.keys bs
        new = BitId $ 1 + lastBit

        bsF = DM.insert new False bs
        l = LeafNode (bsF, ms', new)

        bsT = DM.insert new True bs
        r = LeafNode (bsT, ms', new)

transformDynamicLifting :: BitId -> EntangleMonad Bool
transformDynamicLifting i = EntangleMonad res where
    res bs ms = LeafNode (bs, ms, b) where
        b = fromMaybe False $ DM.lookup i bs

-- |mytransformer is the main transformer.
-- it used to extract the needed information from a Quipper circuit.
mytransformer :: Transformer EntangleMonad QubitId BitId
--mytransformer g | trace (show g) False = undefined
mytransformer (T_QGate name _ _ _ _ f) = f g where
    open (Signed _ False) = error "Negative controls are not supported yet"
    open (Signed x True) = x
    g wires g_controls controls = do
        transformGate name wires (map (assumeQubit . open) controls)
        return (wires, g_controls, controls)
mytransformer (T_QRot name _ _ _ _ _ f) = f g where
    open (Signed _ False) = error "Negative controls are not supported yet"
    open (Signed x True) = x
    g wires g_controls controls = do
        transformGate name wires (map (assumeQubit . open) controls)
        return (wires, g_controls, controls)
mytransformer (T_QMeas f) = f transformMeasure
mytransformer (T_DTerm _ f) = f (const $ return ())
mytransformer g = error $ "Gate \"" ++ show g ++ "\" is not supported yet"

assumeQubit :: B_Endpoint QubitId BitId -> QubitId
assumeQubit (Endpoint_Qubit qi) = qi
assumeQubit (Endpoint_Bit _) = error "Using bits as controls is not supported yet"

mydtransformer :: DynamicTransformer EntangleMonad QubitId BitId
mydtransformer = DT mytransformer (error "Boxed circuits are not supported yet") transformDynamicLifting

--instance Show a => Show (ReadWrite a) where
--    show (RW_Return a) = "RW_Return " ++ show a
--    show (RW_Write gate next) = "RW_Write " ++ show gate ++ "\n" ++ show next
--    show (RW_Read wire f) = "RW_Read " ++ show wire ++ "\n" ++ show (f True)
--    show (RW_Subroutine boxId _ next) = "RW_Subroutine " ++ show boxId ++ "\n" ++ show next

showIndented :: Show a => Int -> a -> String
showIndented i x = indent i ++ replace (show x) where
    replace [] = []
    replace ('\n':ss) = '\n' : indent i ++ replace ss
    replace (s:ss) = s : replace ss

-- |buildTree takes a 'Circuit', its arity and returns a tree representing it.
--buildTree :: DBCircuit x -> Int -> CircTree x
buildTree :: Show x => DBCircuit x -> Int -> CircTree x
--buildTree circuit n | trace ("n:\n    " ++ show n ++ "\ncircuit:\n" ++ showIndented 1 circuit) False = undefined
buildTree circuit n = fmap (fst . (\(_, _, x) -> x)) res where
    res = untangle monad DM.empty []
    monad = transform_dbcircuit mydtransformer circuit bindings
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) (QubitId i)) bindings_empty [1..n]
