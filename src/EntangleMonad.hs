{-# LANGUAGE FlexibleContexts #-}

module EntangleMonad where

import Control.Monad
import Data.Foldable hiding (foldr)
import Data.Function
import Data.Matrix
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as DM
import Data.Monoid

import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer

import GatesMatrices
import MatrixExtra
import QTuple
import Utils

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
    | MeasureNode QubitId (CircTree a) (CircTree a)
    | LeafNode a

instance Functor CircTree where
    fmap f x = x >>= (\y -> return $ f y)

instance Monad CircTree where
    return = LeafNode
    (GateNode n qs cs t) >>= f = GateNode n qs cs (t >>= f)
    (MeasureNode q l r) >>= f = MeasureNode q (l >>= f) (r >>= f)
    (LeafNode x) >>= f = f x

instance Foldable CircTree where
    foldMap f (GateNode _ _ _ t) = foldMap f t
    foldMap f (MeasureNode _ l r) = foldMap f l <> foldMap f r
    foldMap f (LeafNode x) = f x

transformGate :: String -> [QubitId] -> [B_Endpoint QubitId BitId] -> EntangleMonad ()
transformGate name qs cs = error "transformGate undefined"

transformMeasure :: QubitId -> EntangleMonad BitId
transformMeasure i = EntangleMonad res where
    res bs = MeasureNode i l r where
        lastBit = foldr (max . unbit) 0 $ DM.keys bs
        new = BitId $ 1 + lastBit
        
        bsT = DM.insert new True bs
        l = LeafNode (bsT, new)
        
        bsF = DM.insert new False bs
        r = LeafNode (bsF, new)

newtype QubitId = QubitId { unqubit :: Int }
newtype BitId = BitId { unbit :: Int } deriving (Eq, Ord)

-- |mytransformer is the main transformer.
-- it used to extract the needed information from a Quipper circuit.
mytransformer :: Transformer EntangleMonad QubitId BitId
mytransformer (T_QGate name a b inv nc f) = f g where
    open (Signed _ False) = error "Negative controls are not supported yet"
    open (Signed x True) = x
    g wires g_controls controls = do
        transformGate name wires (map open controls)
        return (wires, g_controls, controls)
mytransformer (T_QMeas f) = f transformMeasure

-- |buildTree takes a 'Circuit', its arity and returns a tree representing it.
buildTree :: Circuit -> Int -> CircTree ()
buildTree circuit n = fmap (const ()) res where
    res = untangle monad DM.empty
    monad = transform_circuit mytransformer circuit bindings
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) (QubitId i)) bindings_empty [1..n]
