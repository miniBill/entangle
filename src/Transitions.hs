{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transitions where

import           Quipper
import           Quipper.Circuit
import           Quipper.Monad

import           BitQubitId
import           EntangleMonad
import           GatesMatrices
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

instance Show v => Show (Transitions m v) where
    show (Transitions from dests) = "[Transitions trFromState=" ++ show from ++ " trDestinations=" ++ show dests ++ "]"

data Transition m v = Transition {
    trMatrix  :: Maybe (m v),
    trToState :: StateName
}

instance (Show v) => Show (Transition m v) where
    show (Transition _ to) = "[Transition trMatrix=... trToState=" ++ show to ++ "]"

type QubitCount = QubitId
type ControlCount = QubitId

-- |circMatrices takes a function returning a value in the 'Circ' monad,
-- and calculates the list of QPMC transitions needed to represent it.
--circMatrices :: (Num v, Floating v, QTuple a) => (a -> Circ b) -> [Transitions v]
circMatrices :: (Num v, Floating v, QTuple a, Show b, GMatrix m v) => (b -> [Transition m v]) -> (a -> Circ b) -> [Transitions m v]
circMatrices final = treeToTransitions final . circToTree

--circToTree :: QTuple a => (a -> Circ b) -> CircTree b
circToTree :: (Show b, QTuple a) => (a -> Circ b) -> CircTree b
circToTree mcirc = tree where
    arg = tupleFromList $ map qubit_of_wire [1..]
    circ = extract_general arity_empty (mcirc arg)
    argsLength = tupleSize arg
    tree = buildTree circ argsLength

treeToTransitions :: (Num v, Floating v, Show b, GMatrix m v) => (b -> [Transition m v]) -> CircTree b -> [Transitions m v]
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
    go sn@(StateName i bs) (MeasureNode qi b l r) = Transitions sn [lt, rt] : go ls l ++ go rs r where
        lmat = between (pred qi) (measureMatrix UL) (qubit_max - qi)
        ls = StateName (succ i) (bs ++ [False])
        lt = Transition (Just lmat) ls

        rmat = between (pred qi) (measureMatrix BR) (qubit_max - qi)
        rs = StateName (succ i) (bs ++ [True])
        rt = Transition (Just rmat) rs

-- |getWires returns the qubit numbers involved in a gate.
--getWires :: CircTree a -> [QubitId]
getWires :: Show a => CircTree a -> [QubitId]
getWires (LeafNode _)          = []
getWires (GateNode _ qs cs c)  = qs ++ cs ++ getWires c
getWires (MeasureNode q _ l r) = q : getWires l ++ getWires r

-- |sw q t is a function that swaps q and t
sw :: Eq a => a -> a -> (a -> a)
sw q t x | x == q = t
         | x == t = q
         | otherwise = x

-- |gateToMatrix takes the number of qubits, a gate data and returns the matrix needed to represent it.
gateToMatrix :: (Num a, Floating a, GMatrix m a) => QubitCount -> String -> [QubitId] -> [QubitId] -> m a
gateToMatrix size name qs cs = moving size gsw m where
    wires = cs ++ qs
    mi = foldr min size wires
    gsw = reverse $ generateSwaps wires [mi..]
    lc = qubitId $ length cs
    lq = qubitId $ length qs
    m = between (pred mi) (nameToMatrix lc lq name) (pred $ size - (mi + lc + lq))

-- |generateSwaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place.
-- Algebrically this is a decomposition of a generic permutation into swaps.
generateSwaps :: Eq t => [t] -> [t] -> [(t, t)]
generateSwaps [] _ = []
generateSwaps (q:qs) (t:ts)
    | q == t = generateSwaps qs ts
    | otherwise = (q, t) : generateSwaps (map (sw q t) qs) ts
generateSwaps _ _ = error "Unbalanced lists passed to generateSwaps"

data MeasureKind = UL | BR

-- |measureMatrix is the measure matrix
-- measureMatrix True is (1 0; 0 0) whereas measureMatrix False is (0 0; 0 1)
measureMatrix :: (Num a, GMatrix m a) => MeasureKind -> m a
measureMatrix k =
    let
        gen UL 1 1 = 1
        gen BR 2 2 = 1
        gen _ _ _  = 0
    in
        matrix 2 2 (gen k)

-- |nameToMatrix is the matrix for the given named gate.
-- It returns a matrix with an identity in the top left
-- and the action in the bottom right.
nameToMatrix :: (Num a, Floating a, GMatrix m a) => ControlCount -> QubitCount -> String -> m a
nameToMatrix controlCount qubitCount name =
    let
        total_size = toSize (controlCount + qubitCount)
        small_size = if qubitCount == 0 then 0 else toSize qubitCount
        big_size = total_size - small_size
    in
          (identity big_size        <|> zero big_size small_size)
                                    <->
          (zero small_size big_size <|> matrix small_size small_size (nameToGen name))


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
    | otherwise = between (pred n) (matrix 4 4 swapMatrix) (size - m)

-- |between takes a matrix and applies it to the chosen qubits,
-- without modifying the other ones
between :: GMatrix m a => QubitCount -> m a -> QubitCount -> m a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ toSize b
    after  = identity $ toSize a
