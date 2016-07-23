{-# LANGUAGE FlexibleContexts #-}

module EntangleMonad where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Matrix

import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer

import GatesMatrices
import MatrixExtra
import QTuple
import Utils

-- |'Transformed' contains the minimal information needed
-- to translate a quantum gate to QPMC.
data Transformed = Gate String [Int] [Int]
                 | Measure Int deriving Show

-- |mytransformer is the main transformer.
-- it used to extract the needed information from a Quipper circuit.
mytransformer :: Transformer (Writer [Transformed]) Int Int
mytransformer (T_QGate name a b inv nc f) = f g where
    open = map (\(Signed x _) -> endpoint_to_int x)
    endpoint_to_int (Endpoint_Qubit x) = x
    endpoint_to_int (Endpoint_Bit x) = -x
    g gates g_controls controls = do
        tell [Gate name (open controls) gates]
        return (gates, g_controls, controls)
mytransformer (T_QMeas f) = f g where
    g qubit = do
        tell [Measure qubit]
        return qubit

-- |transformed takes a 'Circuit' with its arity and returns the list of 'Transformed' gates.
transformed :: Circuit -> Int -> [Transformed]
transformed circuit n = execWriter . void $ transform_circuit mytransformer circuit bindings where
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) i) bindings_empty [1..n]

-- |getGates returns the qubit numbers involved in a gate.
getGates :: Transformed -> [Int]
getGates (Gate _ cs qs) = qs ++ cs
getGates (Measure q) = [q]

-- circMatrixes :: [(Int, [TransGate])] -> [(Int, Matrix)]
-- f :: [TransGate] -> Matrix
-- size :: Int = 2 ^ qubit
-- qubit_max :: Int

-- |'Transitions' represent the list of transitions from a state to other states,
-- together with the respective matrices.
data Transitions v = Transitions {
    trFromState :: StateName,                  -- ^ state from which the transitions start
    trDestinations :: [(StateName, Matrix v)]  -- ^ destinations and matrices
}

data StateName = StateName {
    snId :: Integer,         -- ^ state id
    snBs :: [Bool]           -- ^ boolean values in the state
    } deriving (Ord, Eq)

instance Show StateName where
    show (StateName i bs) = show i ++ "_" ++ map (\b -> if b then 'T' else 'F') (reverse bs)

-- |circMatrices takes a function returning a value in the 'Circ' monad,
-- and calculates the list of QPMC transitions needed to represent it.
circMatrices :: (Num a, Floating a, QTuple b) => (b -> Circ c) -> [Transitions a]
circMatrices circ = undefined where -- concat $ evalState result (0, 1)
    arg = tupleFromList $ map qubit_of_wire [1..]
    extracted_n = size arg
    foo = circ arg
    {-result = f gate_list
    --gate_list = circ_transform circ
    size = 2 ^ qubit_max
    qubit_max = maximum $ concatMap getGates gate_list
    f :: [Transformed] -> State (Int, Int) [(Int, [(Matrix Double, Int)])]
    f gs = do
        (from, to) <- get -- from is inclusive, to exclusive
        let tree_size = to - from
        let choices = map (gateToMatrices qubit_max) gs
        let paths = makeTuples choices
        let ms = map (foldr (*) (identity size)) paths
        put (to, to + tree_size * length paths)
        return $ do
            i <- [0..tree_size - 1]
            let ts = do
                (j, m) <- zip [0..] ms
                return (m, i * length paths + to + j)
                return (i + from, ts)-}

-- |generateSwaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place
generateSwaps [] _ = []
generateSwaps (q:qs) (t:ts)
    | q == t = generateSwaps qs ts
    | otherwise = (q, t) : generateSwaps (map (sw q t) qs) ts

-- |sw q t is a function that swaps q and t
sw q t x | x == q = t
         | x == t = q
         | otherwise = x

-- |gateToMatrices takes a single gate and returns the list of matrices needed to represent it.
gateToMatrices :: (Num a, Floating a) => Int -> Transformed -> [Matrix a]
gateToMatrices size (Gate name cs qs) = [moving size sw m] where
    mi = foldr min size (cs ++ qs)
    sw = reverse $ generateSwaps (cs ++ qs) [mi..]
    lc = length cs
    lq = length qs
    m = between (mi-1) (nameToMatrix lc lq name) (size - (mi + lc + lq - 1))
gateToMatrices size (Measure q) = [m, m'] where
    m = between (q-1) (measureMatrix 1) (size - q)
    m' = between (q-1) (measureMatrix 2) (size - q)

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
