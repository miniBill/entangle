{-# LANGUAGE FlexibleInstances #-}
module Main where

import SqMath
import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer
import Debug.Trace

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Matrix
import Data.Ratio
import qualified Data.Map.Strict as Map

data StrataState a b = StrataState {
    strataState :: Map.Map b Int,
    composition :: Map.Map Int [a] }

stratify :: Ord b => (a -> [b]) -> [a] -> [[a]]
stratify f = map snd . Map.toAscList . composition . foldr stratify' emptyState . map (id &&& f) . reverse where
	emptyState = StrataState Map.empty Map.empty

stratify' :: Ord b => (a, [b]) -> StrataState a b -> StrataState a b
stratify' (e, fe) (StrataState strata old) = StrataState newstrata new where
        newstratum = stratum fe strata
        newstrata = foldr (flip Map.insert $ newstratum + 1) strata fe
        new = Map.insertWith (++) newstratum [e] old

stratum :: Ord b => [b] -> Map.Map b Int -> Int
stratum x s = foldr (max .  stratum') 0 x where
    stratum' b = Map.findWithDefault 0 b s

strashow :: Show a => [[a]] -> String
strashow xs = foldr (\ x y -> show' x ++ "\n" ++ y) "" $ zip [1..] xs where
    show' (s, es) = show (s :: Int) ++ ": " ++ (foldr1 (\e o -> e ++ ", " ++ o) $ map show es)

data Transformed = Gate String [Int] [Int]
                 | Measure Int deriving Show

mytransformer :: Transformer (Writer [Transformed]) Int Int
mytransformer (T_QGate name a b inv nc f) = f g where
    open = map (\(Signed x _) -> endpoint_to_int x)
    endpoint_to_int (Endpoint_Qubit x) = x
    endpoint_to_int (Endpoint_Bit x) = -x
    g gates g_controls controls = do
        tell [Gate name gates (open controls)]
        return (gates, g_controls, controls)
mytransformer (T_QMeas f) = f g where
    g qubit = do
        tell [Measure qubit]
        return qubit

class Tuple a where
    size :: a -> Int
    tupleFromList :: [Qubit] -> a

instance Tuple Qubit where
    size _ = 1
    tupleFromList = head

instance Tuple (Qubit, Qubit) where
    size _ = 2
    tupleFromList (q1:q2:_) = (q1, q2)

instance Tuple (Qubit, Qubit, Qubit) where
    size _ = 3
    tupleFromList (q1:q2:q3:_) = (q1, q2, q3)

instance Tuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    size _ = 6
    tupleFromList (q1:q2:q3:q4:q5:q6:_) = (q1, q2, q3, q4, q5, q6)

extract :: Tuple a => (a -> Circ b) -> (Circuit, Int)
extract circ = (extracted, extracted_n) where
    arg = tupleFromList $ map qubit_of_wire [1..]
    extracted_n = size arg
    ((extracted, _), _) = extract_simple id arity_empty (circ arg)

transformed :: Circuit -> Int -> Writer [Transformed] (Bindings Int Int)
transformed circuit n = transform_circuit mytransformer circuit bindings where
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) i) bindings_empty [1..n]

circ_stratify :: Tuple a => (a -> Circ b) -> [[Transformed]]
circ_stratify circ = stratify get_gates $ snd $ runWriter $ transformed extracted extracted_n where
    (extracted, extracted_n) = extract circ

get_gates :: Transformed -> [Int]
get_gates (Gate _ qs cs) = qs ++ cs
get_gates (Measure q) = [q]

-- circ_matrixes :: [(Int, [TransGate])] -> [(Int, Matrix)]
-- f :: [TransGate] -> Matrix
-- size :: Int = 2 ^ qubit
-- qubit_max :: Int
-- stratified :: [(Int, [TransGate])]

-- make_tuples takes as input a list xs.
-- each element x of xs represents a list of possibilities.
-- make_tuples returns lists whose elements are chosen, in order, from each x
-- for example make_tuples [[1, 2], [3, 4]] will return [[1, 3], [1, 4], [2, 3], [2, 4]]
-- you can view the results as the possible "paths" through xs
--
-- make_tuples [[a,b,c],[d,e,f],[g,h,i]]
--
-- a    /- d -\      g
--     /       \
-- b -/    e    \    h
--               \
-- c       f      \- i -> [b, d, i]
--
make_tuples :: [[a]] -> [[a]]
make_tuples [] = [[]]
make_tuples (xs:yss) = do
    x <- xs
    p <- make_tuples yss
    return (x:p)

type Transitions a = (Int, [(Matrix a, Int)]) -- (from_state, [(transformation, to_state)])

circ_matrixes :: (Num a, Floating a, Tuple b) => (b -> Circ c) -> [Transitions a]
circ_matrixes circ = concat $ evalState result (0, 1) where
    result = mapM f stratified
    stratified = circ_stratify circ
    size = 2 ^ qubit_max
    gate_list = concat stratified
    qubit_max = maximum $ concatMap get_gates gate_list
    f gs = do
        (from, to) <- get -- from is inclusive, to exclusive
        let tree_size = to - from
        let choices = map (gate_to_matrixes qubit_max) gs
        let paths = make_tuples choices
        let ms = map (foldr (*) (identity size)) paths
        put (to, to + tree_size * length paths)
        return $ do
            i <- [0..tree_size - 1]
            let ts = do
                (j, m) <- zip [0..] ms
                return (m, i * length paths + to + j)
            return (i + from, ts)

gate_to_matrixes :: (Num a, Floating a) => Int -> Transformed -> [Matrix a]
gate_to_matrixes size (Gate name [q] []) = [moving size sw m] where
    q' = q
    sw = []
    m = between (q'-1) (name_to_matrix 1 0 name) (size - q')
gate_to_matrixes size (Gate name [q] [c]) = [moving size sw m] where
    c' = min q c
    q' = max q c
    sw = (q', c'+1) : (if q < c then [(q, c)] else [])
    m = between (c'-1) (name_to_matrix 1 1 name) (size - q')
gate_to_matrixes size (Measure q) = [m, m'] where
    m = between (q-1) (measure_matrix 1) (size - q)
    m' = between (q-1) (measure_matrix 2) (size - q)

measure_matrix :: (Num a) => Int -> Matrix a
measure_matrix i = matrix 2 2 gen where
  gen (x, y) | x == i && y == i = 1
  gen _ = 0

measure_matrix' :: (Num a) => Int -> Matrix a
measure_matrix' i = matrix 2 2 gen where
  gen (x, y) | x == i && y == i = 1
  gen _ = 0

name_to_matrix :: (Num a, Floating a) => Int -> Int -> String -> Matrix a
name_to_matrix 1 0 "not" = not_matrix
name_to_matrix 1 0 "H" = hadamard_matrix
name_to_matrix 1 1 "not" = cnot_matrix

hadamard_matrix :: (Num a, Floating a) => Matrix a
hadamard_matrix = (1 / sqrt 2) `scaleMatrix` matrix 2 2 gen where
    gen (2, 2) = -1
    gen _ = 1

not_matrix :: Num a => Matrix a
not_matrix = matrix 2 2 gen where
    gen (1, 2) = 1
    gen (2, 1) = 1
    gen _ = 0

cnot_matrix :: Num a => Matrix a
cnot_matrix = matrix 4 4 gen where
    gen (1, 1) = 1
    gen (2, 2) = 1
    gen (3, 4) = 1
    gen (4, 3) = 1
    gen _ = 0

swap_matrix :: Num a => Matrix a
swap_matrix = matrix 4 4 gen where
    gen (1, 1) = 1
    gen (2, 3) = 1
    gen (3, 2) = 1
    gen (4, 4) = 1
    gen _ = 0

moving :: Num a => Int -> [(Int, Int)] -> Matrix a -> Matrix a
moving size moves m = back * m * forth where
    forth = move size moves
    back  = move size $ reverse moves

move :: Num a => Int -> [(Int, Int)] -> Matrix a
move size ms = foldr f (identity (2 ^ size)) ms where
    f (t1, t2) m = swap_to_matrix size t1 t2 * m

swap_to_matrix :: Num a => Int -> Int -> Int -> Matrix a
swap_to_matrix size n m | n > m = swap_to_matrix size m n
                        | n == m = identity (2 ^ size)
                        | n < m - 1 = swap_to_matrix size n (m - 1) * swap_to_matrix size (m - 1) m
                        -- otherwise: n == m - 1
                        | otherwise = between (n - 1) swap_matrix (size - m) where

between :: Num a => Int -> Matrix a -> Int -> Matrix a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ 2 ^ b
    after  = identity $ 2 ^ a

kronecker :: Num a => Matrix a -> Matrix a -> Matrix a
kronecker a b = matrix (ra * rb) (ca * cb) (uncurry gen) where
    ra = nrows a
    rb = nrows b
    ca = ncols a
    cb = ncols b
    gen r c = ae * be where
        ae = a ! (ar, ac)
        ar = 1 + (r - 1) `div` rb
        ac = 1 + (c - 1) `div` cb
        be = b ! (br, bc)
        br = 1 + (r - 1) `mod` rb
        bc = 1 + (c - 1) `mod` cb

--- Esempio ---
myfourthcirc :: Qubit -> Circ Qubit
myfourthcirc q1 = do
    hadamard q1
    qnot q1
    hadamard q1
    qnot q1
    return q1

mythirdcirc :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
mythirdcirc (q1, q2, q3) = do
    qnot_at q2 `controlled` q1
    qnot_at q2 `controlled` q3
    qnot_at q2 `controlled` q1
    qnot_at q2 `controlled` q3
    return (q1, q2, q3)

myothercirc :: Qubit -> Circ Qubit
myothercirc q1 = do
    hadamard q1
    hadamard q1
    return q1

mycirc :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
mycirc (q1, q2, q3, q4, q5, q6) = do
    qnot_at q1 `controlled` q2
    qnot_at q2 `controlled` q3
    qnot_at q5 `controlled` q6
    qnot_at q4 `controlled` q5
    qnot_at q3 `controlled` q4
    return (q1, q2, q3, q4, q5, q6)

deutsch :: (Qubit, Qubit) -> Circ Bit
deutsch (q1, q2) = do
    hadamard q1
    hadamard q2
    qnot_at q2 `controlled` q1
    hadamard q1
    measure q1


oneq :: Qubit -> Circ Qubit
oneq q1 = do
  hadamard_at q1
  return q1


double_meas :: (Qubit, Qubit) -> Circ (Bit, Bit)
double_meas (q1, q2) = measure (q1, q2)

strange :: (Qubit, Qubit) -> Circ (Bit, Bit)
strange (q1, q2) = do
  c2 <- measure q2
  hadamard q1
  hadamard q1
  c1 <- measure q1
  return (c1, c2)

--- Converter ---
to_qmc :: [Transitions Expr] -> String
to_qmc ts = "qmc\n"
--           const matrix asdf = [1,2;3,4];
--           "mf2so([1,0,0,0; 0,0,0,0; 0,0,0,0; 0,0,0,0])
          ++ concatMap matrix_to_qmc (concatMap snd ts)
          ++ "module test\n"
          ++ "  s: [0.." ++ show (foldr max 0 named) ++ "] init 0;\n"
          ++ concatMap transition_to_qmc ts
          ++ concatMap final_to_qmc finals
          ++ "endmodule" where
    named = concatMap (map snd . snd) ts
    finals = named \\ map fst ts

final_to_qmc :: Int -> String
final_to_qmc s = "  [] (s = " ++ show s ++ ") -> (s' = " ++ show s ++ ");\n"

matrix_to_qmc :: Show a => (Matrix a, Int) -> String
matrix_to_qmc (m, t) = "const matrix A" ++ show t ++ " = [" ++ inner ++ "];\n" where
    inner = concat $ intersperse ";" $ map sl $ toLists m
    sl l = concat $ intersperse "," $ map show l

transition_to_qmc :: Transitions a -> String
transition_to_qmc (f, ts) = "  [] (s = " ++ show f ++ ")"
              ++ " -> "
              ++ concat (intersperse " + " (map transition_to_qmc' ts))
              ++ ";\n"

transition_to_qmc' :: (Matrix a, Int) -> String
transition_to_qmc' (_, t) = "<<A" ++ show t
              ++ ">> : "
              ++ "(s' = " ++ show t ++ ")"

full_out :: Tuple a => (a -> Circ b) -> IO ()
full_out c = do
--    putStr "---\n"
--    print $ circ_matrixes c
    putStr "---\n"
    putStr $ strashow $ circ_stratify c
    putStr "---\n"
    putStrLn $ to_qmc $ circ_matrixes c
    putStr "---\n"

main = do
  full_out deutsch
  --full_out oneq
  --full_out double_meas
  full_out strange

