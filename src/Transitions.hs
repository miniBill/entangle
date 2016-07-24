module Transitions where

import Data.Matrix

import Quipper
import Quipper.Circuit
import Quipper.Monad

import EntangleMonad
import GatesMatrices
import MatrixExtra
import QTuple

data StateName = StateName {
    snId :: Integer,  -- ^ state id
    snBs :: [Bool]    -- ^ boolean values in the state
} deriving (Ord, Eq)

instance Show StateName where
    show (StateName i bs) = show i ++ "_" ++ map (\b -> if b then 'T' else 'F') (reverse bs)

data Transitions v = Transitions {
    trFromState :: StateName,
    trDestinations :: [Transition v]
}

instance Show v => Show (Transitions v) where
    show (Transitions from dests) = "[Transitions trFromState=" ++ show from ++ " trDestinations=" ++ show dests ++ "]"

data Transition v = Transition {
    trMatrix :: Matrix v,
    trToState :: StateName
}

instance (Show v) => Show (Transition v) where
    show (Transition m to) = "[Transition trMatrix=... trToState=" ++ show to ++ "]"

-- |circMatrices takes a function returning a value in the 'Circ' monad,
-- and calculates the list of QPMC transitions needed to represent it.
--circMatrices :: (Num v, Floating v, QTuple a) => (a -> Circ b) -> [Transitions v]
circMatrices :: (Num v, Floating v, QTuple a, Show b) => (a -> Circ b) -> [Transitions v]
circMatrices = treeToTransitions . circToTree where

--circToTree :: QTuple a => (a -> Circ b) -> CircTree b
circToTree :: Show b => QTuple a => (a -> Circ b) -> CircTree b
circToTree mcirc = tree where
    arg = tupleFromList $ map qubit_of_wire [1..]
    circ = extract_general arity_empty (mcirc arg)
    argsLength = tupleSize arg
    tree = buildTree circ argsLength

treeToTransitions :: (Num v, Floating v, Show b) => CircTree b -> [Transitions v]
treeToTransitions (LeafNode _) = []
--     result = f gate_list
--     gate_list = buildTree circ
--     size = 2 ^ qubit_max
--     qubit_max = maximum $ concatMap getGates gate_list
--     f :: [Transformed] -> State (Int, Int) [(Int, [(Matrix Double, Int)])]
--     f gs = do
--         (from, to) <- get -- from is inclusive, to exclusive
--         let tree_size = to - from
--         let choices = map (gateToMatrices qubit_max) gs
--         let paths = makeTuples choices
--         let ms = map (foldr (*) (identity size)) paths
--         put (to, to + tree_size * length paths)
--         return $ do
--             i <- [0..tree_size - 1]
--             let ts = do
--                 (j, m) <- zip [0..] ms
--                 return (m, i * length paths + to + j)
--                 return (i + from, ts)

-- |getGates returns the qubit numbers involved in a gate.
-- getGates :: Transformed -> [Int]
-- getGates (Gate _ cs qs) = qs ++ cs
-- getGates (Measure q) = [q]

-- |generateSwaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place.
-- Algebrically this is a decomposition of a generic permutation into swaps.
generateSwaps :: Eq t => [t] -> [t] -> [(t, t)]
generateSwaps [] _ = []
generateSwaps (q:qs) (t:ts)
    | q == t = generateSwaps qs ts
    | otherwise = (q, t) : generateSwaps (map (sw q t) qs) ts
generateSwaps _ _ = error "Unbalanced lists passed to generateSwaps"

-- |sw q t is a function that swaps q and t
sw :: Eq a => a -> a -> a -> a
sw q t x | x == q = t
         | x == t = q
         | otherwise = x

-- |gateToMatrices takes a single gate and returns the list of matrices needed to represent it.
-- gateToMatrices :: (Num a, Floating a) => Int -> Transformed -> [Matrix a]
-- gateToMatrices size (Gate name cs qs) = [moving size sw m] where
--     mi = foldr min size (cs ++ qs)
--     sw = reverse $ generateSwaps (cs ++ qs) [mi..]
--     lc = length cs
--     lq = length qs
--     m = between (mi-1) (nameToMatrix lc lq name) (size - (mi + lc + lq - 1))
-- gateToMatrices size (Measure q) = [m, m'] where
--     m = between (q-1) (measureMatrix 1) (size - q)
--     m' = between (q-1) (measureMatrix 2) (size - q)

-- |measureMatrix is the matrix for the gate measuring the i-th qubit
measureMatrix :: (Num a) => Int -> Matrix a
measureMatrix i = matrix 2 2 gen where
  gen (x, y) | x == i && y == i = 1
  gen _ = 0

-- |nameToMatrix is the matrix for the given named gate.
-- It returns a matrix with an identity in the top left
-- and the action in the bottom right.
nameToMatrix :: (Num a, Floating a) => Int -> Int -> String -> Matrix a
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
moving :: Num a => Int -> [(Int, Int)] -> Matrix a -> Matrix a
moving size moves m = back * m * forth where
    forth = move size moves
    back  = move size $ reverse moves

-- |move is the matrix that moves the chosen qubits
move :: Num a => Int -> [(Int, Int)] -> Matrix a
move size = foldr f (identity (2 ^ size)) where
    f (t1, t2) m = swapToMatrix size t1 t2 * m

-- |swapToMatrix is the matrix swapping the chosen qubits
swapToMatrix :: Num a => Int -> Int -> Int -> Matrix a
swapToMatrix size n m | n > m = swapToMatrix size m n
                        | n == m = identity (2 ^ size)
                        | n < m - 1 = swapToMatrix size n (m - 1) * swapToMatrix size (m - 1) m
                        -- otherwise: n == m - 1
                        | otherwise = between (n - 1) (matrix 4 4 $ uncurry swapMatrix) (size - m)

-- |between takes a matrix and applies it to the chosen qubits,
-- without modifying the other ones
between :: Num a => Int -> Matrix a -> Int -> Matrix a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ 2 ^ b
    after  = identity $ 2 ^ a

