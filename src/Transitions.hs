{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transitions where

import           Quipper
import           Quipper.Circuit
import           Quipper.Monad

import           BitQubitId
import           Complex
import           EntangleMonad
import           Expr
import qualified GatesMatrices
import           MatrixExtra
import           QTuple

data StateName = StateName {
    snId :: Integer,  -- ^ state id
    snBs :: [Bool]    -- ^ boolean values in the state
} deriving Eq

instance Ord StateName where
    compare a b
        | snId a < snId b = LT
        | snId a > snId b = GT
        | otherwise = compare' (snBs a) (snBs b) where
            compare' [] []              = EQ
            compare' [] _               = LT
            compare' _ []               = GT
            compare' (True:_) (False:_) = LT
            compare' (False:_) (True:_) = GT
            compare' (_:as) (_:bs)      = compare' as bs

instance Show StateName where
    show (StateName i bs) = show i ++ if null bs then "" else "_" ++ map (\b -> if b then 'T' else 'F') (reverse bs)

data Transitions m v = Transitions {
    trFromState    :: StateName,
    trDestinations :: [Transition m v]
}

instance Show (Transitions m v) where
    show (Transitions from dests) = "[Transitions trFromState=" ++ show from ++ " trDestinations=" ++ show dests ++ "]"

data Transition m v = Transition {
    trMatrix  :: Maybe (m (Complex v)),
    trToState :: StateName
}

instance Show (Transition m v) where
    show (Transition _ to) = "[Transition trMatrix=... trToState=" ++ show to ++ "]"

type QubitCount = QubitId
type ControlCount = QubitId

-- |circMatrices takes a function returning a value in the 'Circ' monad,
-- and calculates the list of QPMC transitions needed to represent it.
--circMatrices :: (Floating a, FromDouble a, QTuple q) => (q -> Circ b) -> [Transitions a]
circMatrices :: (Floating a, FromDouble a, QTuple q, Show b, GCMatrix m a) => (b -> [Transition m a]) -> (q -> Circ b) -> [Transitions m a]
circMatrices final = treeToTransitions final . circToTree

--circToTree :: QTuple a => (a -> Circ b) -> CircTree b
circToTree :: (Show b, QTuple a) => (a -> Circ b) -> CircTree b
circToTree mcirc = tree where
    arg = tupleFromList $ map qubit_of_wire [1..]
    circ = extract_general arity_empty (mcirc arg)
    argsLength = tupleSize arg
    tree = buildTree circ argsLength

treeToTransitions :: (Fractional a, Floating a, FromDouble a, Show b, GCMatrix m a) => (b -> [Transition m a]) -> CircTree b -> [Transitions m a]
treeToTransitions final t = go (StateName 0 []) t where
    wires :: [QubitId]
    wires = getWires t
    qubit_max :: QubitId
    qubit_max = foldr max minBound wires
    go sn (LeafNode x) = if null f then [] else [Transitions sn f] where
        f = final x
    go sn@(StateName i bs) (GateNode name qs cts c) = Transitions sn [tr] : go state' c where
        tr = Transition (Just mat) state'
        mat = gateToMatrix qubit_max name qs cts
        state' = StateName (succ i) bs
    go sn@(StateName i bs) (ParameterizedGateNode name t qs cts c) = Transitions sn [tr] : go state' c where
        tr = Transition (Just mat) state'
        mat = parameterizedGateToMatrix qubit_max name t qs cts
        state' = StateName (succ i) bs
    go sn@(StateName i bs) (MeasureNode qi b l r) = Transitions sn [lt, rt] : go ls l ++ go rs r where
        lmat = between (pred qi) (MatrixExtra.measure UL) (qubit_max - qi)
        ls = StateName (succ i) (bs ++ [False])
        lt = Transition (Just lmat) ls

        rmat = between (pred qi) (MatrixExtra.measure BR) (qubit_max - qi)
        rs = StateName (succ i) (bs ++ [True])
        rt = Transition (Just rmat) rs

-- |getWires returns the qubit numbers involved in a gate.
--getWires :: CircTree a -> [QubitId]
getWires :: Show a => CircTree a -> [QubitId]
getWires (LeafNode _)                        = []
getWires (GateNode _ qs cs c)                = qs ++ cs ++ getWires c
getWires (ParameterizedGateNode _ _ qs cs c) = qs ++ cs ++ getWires c
getWires (MeasureNode q _ l r)               = q : getWires l ++ getWires r

-- |sw q t is a function that swaps q and t
sw :: Eq a => a -> a -> (a -> a)
sw q t x | x == q = t
         | x == t = q
         | otherwise = x

-- |gateToMatrix takes the total number of qubits, a gate data and returns the matrix needed to represent it.
gateToMatrix :: (Fractional a, Floating a, GCMatrix m a) => QubitCount -> String -> [QubitId] -> [QubitId] -> m (Complex a)
gateToMatrix size name qs cs =
    let
        wires = cs ++ qs
        mi = minimum wires
        swaps = reverse $ generateSwaps wires [mi..]
        controlCount = qubitId $ length cs
        qubitCount = qubitId $ length qs
        ma = pred $ mi + controlCount + qubitCount
        l = pred mi
        m = nameToMatrix controlCount qubitCount name
        r = size - ma
        mat = between l m r
    in
        moving size swaps mat

-- |parameterizedGateToMatrix takes the total number of qubits, a gate data and returns the matrix needed to represent it.
parameterizedGateToMatrix :: (Fractional a, Floating a, FromDouble a, GCMatrix m a) => QubitCount -> String -> Double -> [QubitId] -> [QubitId] -> m (Complex a)
parameterizedGateToMatrix size name t qs cs =
    let
        wires = cs ++ qs
        mi = minimum wires
        swaps = reverse $ generateSwaps wires [mi..]
        controlCount = qubitId $ length cs
        qubitCount = qubitId $ length qs
        ma = pred $ mi + controlCount + qubitCount
        l = pred mi
        m = nameToParameterizedMatrix t controlCount qubitCount name
        r = size - ma
        mat = between l m r
    in
        moving size swaps mat

-- |generateSwaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place.
-- Algebrically this is a decomposition of a generic permutation into swaps.
generateSwaps :: Eq t => [t] -> [t] -> [(t, t)]
generateSwaps [] _ = []
generateSwaps _ [] = []
generateSwaps (q:qs) (t:ts)
    | q == t = generateSwaps qs ts
    | otherwise = (q, t) : generateSwaps (map (sw q t) qs) ts

-- |nameToMatrix is the matrix for the given named gate.
-- It returns a matrix with an identity in the top left
-- and the action in the bottom right.
nameToMatrix :: (Fractional a, Floating a, GCMatrix m a) => ControlCount -> QubitCount -> String -> m (Complex a)
nameToMatrix controlCount qubitCount name =
    let
        total_size = toSize (controlCount + qubitCount)
        small_size = if qubitCount == 0 then 0 else toSize qubitCount
        big_size = total_size - small_size
        active = GatesMatrices.nameToMatrix name
    in
        if big_size == 0
            then active
            else
                (identity big_size        <|> zero big_size small_size)
                                          <->
                (zero small_size big_size <|> active)

-- |nameToParameterizedMatrix is the matrix for the given named parameterized gate.
-- It returns a matrix with an identity in the top left
-- and the action in the bottom right.
nameToParameterizedMatrix :: (Fractional a, Floating a, FromDouble a, GCMatrix m a) => Double -> ControlCount -> QubitCount -> String -> m (Complex a)
nameToParameterizedMatrix t controlCount qubitCount name =
    let
        total_size = toSize (controlCount + qubitCount)
        small_size = if qubitCount == 0 then 0 else toSize qubitCount
        big_size = total_size - small_size
        active = GatesMatrices.nameToParameterizedMatrix name t
    in
        if big_size == 0
            then active
            else
                (identity big_size        <|> zero big_size small_size)
                                          <->
                (zero small_size big_size <|> active)

-- |moving returns a matrix representing:
--   * moving the chosen qubits
--   * applying the given matrix
--   * moving the qubits back to their original position
moving :: (Num a, GMatrix m a) => QubitCount -> [(QubitId, QubitId)] -> m a -> m a
moving size moves m = back * m * forth where
    forth = move size moves
    back  = move size $ reverse moves

-- |move is the matrix that moves the chosen qubits
move :: (Num a, GMatrix m a) => QubitCount -> [(QubitId, QubitId)] -> m a
move size = foldr f $ identity (toSize size) where
    f (t1, t2) m = swapToMatrix size t1 t2 * m

-- |swapToMatrix is the matrix swapping the chosen qubits
swapToMatrix :: (Num a, GMatrix m a) => QubitCount -> QubitId -> QubitId -> m a
swapToMatrix size n m
    | n > m = swapToMatrix size m n
    | n == m = identity $ toSize size
    | n < pred m  = swapToMatrix size n (pred m) * swapToMatrix size (pred m) m
    -- otherwise: n == pred m
    | otherwise = between (pred n) MatrixExtra.swap (size - m)

-- |between takes a matrix and applies it to the chosen qubits,
-- without modifying the other ones
between :: GMatrix m a => QubitCount -> m a -> QubitCount -> m a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ toSize b
    after  = identity $ toSize a
