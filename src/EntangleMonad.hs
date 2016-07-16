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
import Stratify
import Tuple

--data EntangleMonad = 

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

-- |extract transforms a function returning a value in the 'Circ' monad
-- into a 'Circuit' that can be processed further.
extract :: Tuple a => (a -> Circ b) -> (Circuit, Int)
extract circ = (extracted, extracted_n) where
    arg = tupleFromList $ map qubit_of_wire [1..]
    extracted_n = size arg
    ((extracted, _), _) = extract_simple id arity_empty (circ arg)

-- |transformed takes a 'Circuit' with its arity and returns the list of 'Transformed' gates.
transformed :: Circuit -> Int -> [Transformed]
transformed circuit n = execWriter . void $ transform_circuit mytransformer circuit bindings where
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) i) bindings_empty [1..n]

-- |circ_stratify takes a function returning a value in the 'Circ' monad,
-- transforms it into a 'Circuit', and stratifies its gates.
circ_stratify :: Tuple a => (a -> Circ b) -> [[Transformed]]
circ_stratify circ = stratify get_gates $ transformed extracted extracted_n where
    (extracted, extracted_n) = extract circ

-- |get_gates returns the qubit numbers involved in a gate.
get_gates :: Transformed -> [Int]
get_gates (Gate _ cs qs) = qs ++ cs
get_gates (Measure q) = [q]

-- circ_matrixes :: [(Int, [TransGate])] -> [(Int, Matrix)]
-- f :: [TransGate] -> Matrix
-- size :: Int = 2 ^ qubit
-- qubit_max :: Int
-- stratified :: [(Int, [TransGate])]

-- |make_tuples takes as input a list xs.
-- each element x of xs represents a list of possibilities.
-- make_tuples returns lists whose elements are chosen, in order, from each x
-- for example make_tuples [[1, 2], [3, 4]] will return [[1, 3], [1, 4], [2, 3], [2, 4]]
-- you can view the results as the possible "paths" through xs.
--
-- > make_tuples [[a,b,c],[d,e,f],[g,h,i]]
--
-- @
-- a    /- d -\\      g
--     /       \\
-- b -/    e    \\    h
--               \\
-- c       f      \\- i -> [b, d, i]
-- @
make_tuples :: [[a]] -> [[a]]
make_tuples [] = [[]]
make_tuples (xs:yss) = do
    x <- xs
    p <- make_tuples yss
    return (x:p)

-- |'Transitions' represent the list of transitions from a state to other states,
-- together with the respective matrices.
type Transitions a = (Int, [(Matrix a, Int)]) -- (from_state, [(transformation, to_state)])

-- |circ_matrices takes a function returning a value in the 'Circ' monad,
-- and calculates the list of QPMC transitions needed to represent it.
circ_matrices :: (Num a, Floating a, Tuple b) => (b -> Circ c) -> [Transitions a]
circ_matrices circ = concat $ evalState result (0, 1) where
    result = mapM f stratified
    stratified = circ_stratify circ
    size = 2 ^ qubit_max
    gate_list = concat stratified
    qubit_max = maximum $ concatMap get_gates gate_list
    f gs = do
        (from, to) <- get -- from is inclusive, to exclusive
        let tree_size = to - from
        let choices = map (gate_to_matrices qubit_max) gs
        let paths = make_tuples choices
        let ms = map (foldr (*) (identity size)) paths
        put (to, to + tree_size * length paths)
        return $ do
            i <- [0..tree_size - 1]
            let ts = do
                (j, m) <- zip [0..] ms
                return (m, i * length paths + to + j)
            return (i + from, ts)

-- |generate_swaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place
generate_swaps [] _ = []
generate_swaps (q:qs) (t:ts)
    | q == t = generate_swaps qs ts
    | otherwise = (q, t) : generate_swaps (map (sw q t) qs) ts

-- |sw q t is a function that swaps q and t
sw q t x | x == q = t
         | x == t = q
         | otherwise = x

-- |gate_to_matrices takes a single gate and returns the list of matrices needed to represent it.
gate_to_matrices :: (Num a, Floating a) => Int -> Transformed -> [Matrix a]
gate_to_matrices size (Gate name cs qs) = [moving size sw m] where
    mi = foldr min size (cs ++ qs)
    sw = reverse $ generate_swaps (cs ++ qs) [mi..]
    lc = length cs
    lq = length qs
    m = between (mi-1) (name_to_matrix lc lq name) (size - (mi + lc + lq - 1))
gate_to_matrices size (Measure q) = [m, m'] where
    m = between (q-1) (measure_matrix 1) (size - q)
    m' = between (q-1) (measure_matrix 2) (size - q)

-- |measure_matrix is the matrix for the gate measuring the i-th qubit
measure_matrix :: (Num a) => Int -> Matrix a
measure_matrix i = matrix 2 2 gen where
  gen (x, y) | x == i && y == i = 1
  gen _ = 0

-- |name_to_matrix is the matrix for the given named gate.
-- It returns a matrix with an identity in the top left
-- and the action in the bottom right.
name_to_matrix :: (Num a, Floating a) => Int -> Int -> String -> Matrix a
name_to_matrix c q n = matrix total_size total_size gen where
    total_size = 2 ^ (c+q)
    small_size = if q == 0 then 0 else 2 ^ q
    big_size = total_size - small_size
    gen (x,y) = gen' i x' y' where
        x' = x - big_size
        y' = y - big_size
        i = x' <= 0 || y' <= 0
    gen' True  x' y' | x' == y' = 1
                     | otherwise = 0
    gen' False x' y' = name_to_gen n x' y'


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
    f (t1, t2) m = swap_to_matrix size t1 t2 * m

-- |swap_to_matrix is the matrix swapping the chosen qubits
swap_to_matrix :: Num a => Int -> Int -> Int -> Matrix a
swap_to_matrix size n m | n > m = swap_to_matrix size m n
                        | n == m = identity (2 ^ size)
                        | n < m - 1 = swap_to_matrix size n (m - 1) * swap_to_matrix size (m - 1) m
                        -- otherwise: n == m - 1
                        | otherwise = between (n - 1) (matrix 4 4 $ uncurry swap_matrix) (size - m)

-- |between takes a matrix and applies it to the chosen qubits,
-- without modifying the other ones
between :: Num a => Int -> Matrix a -> Int -> Matrix a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ 2 ^ b
    after  = identity $ 2 ^ a
