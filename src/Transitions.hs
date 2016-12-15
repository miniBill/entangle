{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transitions where

import           Quipper
import           Quipper.Circuit
import           Quipper.Monad

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
    qubit_max :: Int
    qubit_max = foldr (max . unqubit) 0 wires
    size = 2 ^ qubit_max
    go sn (LeafNode x) = if null f then [] else [Transitions sn f] where
        f = final x
    go sn@(StateName i bs) (GateNode name qs cts c) = Transitions sn [tr] : go state' c where
        tr = Transition (Just mat) state'
        mat = gateToMatrix size name qs cts
        state' = StateName (i+1) bs
    go sn@(StateName i bs) (MeasureNode qi b l r) = Transitions sn [lt, rt] : go ls l ++ go rs r where
        q = unqubit qi

        lmat = between (q-1) (measureMatrix 1) (size - q)
        ls = StateName (i+1) (bs ++ [False])
        lt = Transition (Just lmat) ls

        rmat = between (q-1) (measureMatrix 2) (size - q)
        rs = StateName (i+1) (bs ++ [True])
        rt = Transition (Just rmat) rs

-- |getWires returns the qubit numbers involved in a gate.
--getWires :: CircTree a -> [Int]
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
gateToMatrix :: (Num a, Floating a, GMatrix m a) => Int -> String -> [QubitId] -> [QubitId] -> m a
gateToMatrix size name qs cs = moving size gsw m where
    wires = map unqubit $ cs ++ qs
    mi = foldr min size wires
    gsw = reverse $ generateSwaps wires [mi..]
    lc = length cs
    lq = length qs
    m = between (mi-1) (nameToMatrix lc lq name) (size - (mi + lc + lq - 1))

-- |generateSwaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place.
-- Algebrically this is a decomposition of a generic permutation into swaps.
generateSwaps :: Eq t => [t] -> [t] -> [(t, t)]
generateSwaps [] _ = []
generateSwaps (q:qs) (t:ts)
    | q == t = generateSwaps qs ts
    | otherwise = (q, t) : generateSwaps (map (sw q t) qs) ts
generateSwaps _ _ = error "Unbalanced lists passed to generateSwaps"

-- |measureMatrix is the matrix for the gate measuring the i-th qubit
measureMatrix :: (Num a, GMatrix m a) => Int -> m a
measureMatrix i = matrix 2 2 gen where
  gen (x, y) | x == i && y == i = 1
  gen _      = 0

-- |nameToMatrix is the matrix for the given named gate.
-- It returns a matrix with an identity in the top left
-- and the action in the bottom right.
nameToMatrix :: (Num a, Floating a, GMatrix m a) => Int -> Int -> String -> m a
nameToMatrix c q n = matrix total_size total_size gen where
    total_size = 2 ^ (c+q)
    small_size = if q == 0 then 0 else 2 ^ q
    big_size = total_size - small_size
    gen (x,y) = gen' i x' y' where
        x' = x - big_size
        y' = y - big_size
        i = x' <= 0 || y' <= 0
    gen' True  x' y' | x' == y' = 1
                     | otherwise = 0
    gen' False x' y' = nameToGen n x' y'


-- |moving returns a matrix representing:
--   * moving the chosen qubits
--   * applying the given matrix
--   * moving the qubits back to their original position
moving :: (Num a, GMatrix m a) => Int -> [(Int, Int)] -> m a -> m a
moving size moves m = back * m * forth where
    forth = move size moves
    back  = move size $ reverse moves

-- |move is the matrix that moves the chosen qubits
move :: (Num a, GMatrix m a) => Int -> [(Int, Int)] -> m a
move size = foldr f (identity (2 ^ size)) where
    f (t1, t2) m = swapToMatrix size t1 t2 * m

-- |swapToMatrix is the matrix swapping the chosen qubits
swapToMatrix :: (Num a, GMatrix m a) => Int -> Int -> Int -> m a
swapToMatrix size n m
    | n > m = swapToMatrix size m n
    | n == m = identity (2 ^ size)
    | n < m - 1 = swapToMatrix size n (m - 1) * swapToMatrix size (m - 1) m
    -- otherwise: n == m - 1
    | otherwise = between (n - 1) (matrix 4 4 $ uncurry swapMatrix) (size - m)

-- |between takes a matrix and applies it to the chosen qubits,
-- without modifying the other ones
between :: GMatrix m a => Int -> m a -> Int -> m a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ 2 ^ b
    after  = identity $ 2 ^ a
