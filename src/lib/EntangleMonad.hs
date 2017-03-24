{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EntangleMonad where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable       hiding (concatMap, foldr)
import           Data.List
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as DM
import           Data.Maybe
import           Data.Monoid

--import           Debug.Trace

import           Quipper
import           Quipper.Circuit
import           Quipper.Monad
import           Quipper.Transformer

import           BitQubitId

type BitState = Map BitId Bool

newtype EntangleMonad a = EntangleMonad {
    untangle :: BitState -- ^ state of bits before the operation
             -> [QubitId] -- ^ measured qubits before the operation
             -> Either String (CircTree (BitState, [QubitId], a)) -- ^ tree after the operation
} deriving Functor

instance Applicative EntangleMonad where
    pure = return
    (<*>) = ap

instance Monad EntangleMonad where
    fail err = EntangleMonad (\_ _ -> Left err)
    return x = EntangleMonad (\bs ms -> Right $ LeafNode (bs, ms, x))
    x >>= f = EntangleMonad $ \bs ms ->
        let
            ect = untangle x bs ms
            next (bs', ms', y) = untangle (f y) bs' ms'
        in
            ect >>= \ct -> join <$> mapM next ct

data CircTree a
    = GateNode String [QubitId] [QubitId] (CircTree a) -- ^ name, affected qubits, controls, child
    | ParameterizedGateNode String Double [QubitId] [QubitId] (CircTree a) -- ^ name, parameter, affected qubits, controls, child
    | MeasureNode QubitId BitId (CircTree a) (CircTree a)
    | ResetNode QubitId (CircTree a)
    | LeafNode a
    deriving (Foldable, Traversable, Functor)

instance Applicative CircTree where
    pure = return
    (<*>) = ap

instance Monad CircTree where
    return = LeafNode
    (GateNode n qs cs t) >>= f = GateNode n qs cs (t >>= f)
    (ParameterizedGateNode n p qs cs t) >>= f = ParameterizedGateNode n p qs cs (t >>= f)
    (MeasureNode q b l r) >>= f = MeasureNode q b (l >>= f) (r >>= f)
    (ResetNode q r) >>= f = ResetNode q (r >>= f)
    (LeafNode x) >>= f = f x

instance Show a => Show (CircTree a) where
    show = showTree 0 show' child where
        child (GateNode _ _ _ c)                = [c]
        child (ParameterizedGateNode _ _ _ _ c) = [c]
        child (MeasureNode _ _ l r)             = [l, r]
        child (ResetNode _ c)                   = [c]
        child (LeafNode _)                      = []
        show' (ResetNode q _) = "ResetNode on " ++ show q
        show' (GateNode n qs cs _) = "GateNode \"" ++ n ++ "\" on " ++ qubits ++ (if null controls then "" else " and controls " ++ controls) where
            qubits   = intercalate ", " $ map show qs
            controls = intercalate ", " $ map show cs
        show' (ParameterizedGateNode n t qs cs _) = "ParameterizedGateNode \"" ++ n ++ "\" with parameter " ++ show t ++ " on " ++ qubits ++ (if null controls then "" else " and controls " ++ controls) where
            qubits   = intercalate ", " $ map show qs
            controls = intercalate ", " $ map show cs
        show' (MeasureNode q b _ _) = "MeasureNode on " ++ show q ++ " producing " ++ show b
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
    res bs ms = Right $ GateNode name qs cs $ LeafNode (bs, ms, ())

transformReset :: QubitId -> EntangleMonad ()
transformReset q = EntangleMonad res where
    res bs ms = Right $ ResetNode q $ LeafNode (bs, ms, ())

transformParameterizedGate :: String -> Double -> [QubitId] -> [QubitId] -> EntangleMonad ()
transformParameterizedGate name t qs cs = EntangleMonad res where
    res bs ms = Right $ ParameterizedGateNode name t qs cs $ LeafNode (bs, ms, ())

transformMeasure :: QubitId -> EntangleMonad BitId
--transformMeasure i | trace ("Measuring " ++ show (unQubitID i)) False = undefined
transformMeasure i =
    let
        res bs ms = Right $ MeasureNode i new l r where
            ms' = i : ms

            lastBit = foldr max minBound $ DM.keys bs
            new = succ lastBit

            bsF = DM.insert new False bs
            l = LeafNode (bsF, ms', new)

            bsT = DM.insert new True bs
            r = LeafNode (bsT, ms', new)
    in
        EntangleMonad res

transformDynamicLifting :: BitId -> EntangleMonad Bool
transformDynamicLifting i =
    let
        res bs ms =
            let
                b = fromMaybe False $ DM.lookup i bs
            in
                Right $ LeafNode (bs, ms, b)
    in
        EntangleMonad res

-- |mytransformer is the main transformer.
-- it used to extract the needed information from a Quipper circuit.
mytransformer :: Transformer EntangleMonad QubitId BitId
--mytransformer g | trace (show g) False = undefined
mytransformer (T_QGate "RESET" _ _ _ _ f) = f g where
    g wires [] [] = do
        mapM_ transformReset wires
        return (wires, [], [])
    g _ _ _       = fail "Gate RESET doesn't support controls yet"
mytransformer (T_QGate name _ _ _ _ f) = f g where
    g :: [QubitId] -> [QubitId] -> Ctrls QubitId BitId -> EntangleMonad ([QubitId], [QubitId], Ctrls QubitId BitId)
    g wires g_controls controls = do
        cs <- mapM (assumeQubit <=< open) controls
        transformGate name wires cs
        return (wires, g_controls, controls)
mytransformer (T_QRot name _ _ _ t _ f) = f g where
    g :: [QubitId] -> [QubitId] -> Ctrls QubitId BitId -> EntangleMonad ([QubitId], [QubitId], Ctrls QubitId BitId)
    g wires g_controls controls = do
        cs <- mapM (assumeQubit <=< open) controls
        transformParameterizedGate name t wires cs
        return (wires, g_controls, controls)
mytransformer (T_QMeas f) = f transformMeasure
mytransformer (T_DTerm _ f) = f (const $ return ())
mytransformer g = error $ "Gate \"" ++ show g ++ "\" is not supported yet"

open :: Monad m => Signed (B_Endpoint QubitId BitId) -> m (B_Endpoint QubitId BitId)
open (Signed _ False) = fail "Negative controls are not supported yet"
open (Signed x True)  = return x

assumeQubit :: Monad m => B_Endpoint QubitId BitId -> m QubitId
assumeQubit (Endpoint_Qubit qi) = return qi
assumeQubit (Endpoint_Bit _) = fail "Using bits as controls is not supported yet"

mydtransformer :: DynamicTransformer EntangleMonad QubitId BitId
mydtransformer = DT mytransformer (fail "Boxed circuits are not supported yet") transformDynamicLifting

showIndented :: Show a => Int -> a -> String
showIndented i x = indent i ++ replace (show x) where
    replace []        = []
    replace ('\n':ss) = '\n' : indent i ++ replace ss
    replace (s:ss)    = s : replace ss

-- |buildTree takes a 'Circuit', its arity and returns a tree representing it.
buildTree :: DBCircuit x -> Int -> Either String (CircTree x)
buildTree circuit n =
    let
        bindings :: Bindings QubitId BitId
        bindings = foldr (\i -> bind_qubit (qubit_of_wire i) (qubitId i)) bindings_empty [1..n]
        --monad :: EntangleMonad (x, Bindings QubitId BitId)
        monad = transform_dbcircuit mydtransformer circuit bindings
        --untangled :: Either String (CircTree (x, Bindings QubitId BitId))
        untangled = untangle monad DM.empty []
        clean :: CircTree (a, b, (c, d)) -> CircTree c
        clean = fmap (\(_, _, (res, _)) -> res)
    in
        fmap clean untangled
