module Main where

import SqMath
import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer
import Debug.Trace

import Control.Monad.Writer.Lazy
import Data.List
import Data.Maybe
import Data.Matrix
import Data.Ratio
import qualified Data.Map.Strict as Map

data StrataState a b = StrataState {
    strataState :: Map.Map b b,
    composition :: Map.Map b [a] }

stratify :: (Num b, Ord b) => (a -> [b]) -> [a] -> [(b, [a])]
stratify f = Map.toAscList . stratafold (stratify' f)

stratafold :: (a -> StrataState a b -> StrataState a b) -> [a] -> Map.Map b [a]
stratafold f = composition . foldr f (StrataState Map.empty Map.empty) . reverse

stratify' :: (Ord b, Num b) => (a -> [b]) -> a -> StrataState a b -> StrataState a b
stratify' f e (StrataState strata old) = StrataState newstrata new where
        estratum = stratum (f e) strata
        newstrata = foldr (flip Map.insert $ estratum + 1) strata (f e)
        new = Map.insertWith (++) estratum [e] old

stratum :: (Num b, Ord b) => [b] -> Map.Map b b -> b
stratum x s = foldr (max .  stratum') 0 x where
    stratum' b = Map.findWithDefault 0 b s

strashow :: (Show a, Show b, Ord b, Num b) => [(b, [a])] -> String
strashow xs = foldr (\ x y -> show' x ++ "\n" ++ y) "" $ xs where
    show' (s, es) = show s ++ ": " ++ (foldr1 (\e o -> e ++ ", " ++ o) $ map show es)

mytransformer :: Transformer (Writer [(String, [Int], [Int])]) Int Int
mytransformer (T_QGate name a b inv nc f) = f g where
    open = map (\(Signed x _) -> endpoint_to_int x)
    endpoint_to_int (Endpoint_Qubit x) = x
    endpoint_to_int (Endpoint_Bit x) = -x
    g gates g_controls controls = do
        tell [(name, gates, open controls)]
        return (gates, g_controls, controls)

type TransCirc = [Qubit] -> Circ Int
type TransGate = (String, [Int], [Int])

extract :: TransCirc -> (Circuit, Int)
extract circ = (extracted, extracted_n) where
    ((extracted, _), extracted_n) = extract_simple id arity_empty (circ $ map qubit_of_wire [1..])

transformed :: Circuit -> Int -> Writer [TransGate] (Bindings Int Int)
transformed circuit n = transform_circuit mytransformer circuit bindings where
    bindings = foldr (\i -> bind_qubit (qubit_of_wire i) i) bindings_empty [1..n]

circ_stratify :: TransCirc -> [(Int, [TransGate])]
circ_stratify circ = stratify (\(_,b,c) -> b++c) $ snd $ runWriter $ transformed extracted extracted_n where
    (extracted, extracted_n) = extract circ

-- circ_matrixes :: [(Int, [TransGate])] -> [(Int, Matrix)]
-- f :: [TransGate] -> Matrix
-- size :: Int = 2 ^ qubit
-- qubit_max :: Int
-- stratified :: [(Int, [TransGate])]

circ_matrixes :: (Num a, Floating a) => TransCirc -> [(Int, Matrix a)]
circ_matrixes circ = map (\(s, gs) -> (s, f gs)) stratified where
    stratified = circ_stratify circ
    size = 2 ^ qubit_max
    gate_list = concatMap snd stratified
    qubit_max = maximum $ concatMap (\(_, ms, cs) -> ms ++ cs) gate_list
    f gs = foldr (\g m -> gate_to_matrix qubit_max g * m) (identity size) gs

gate_to_matrix :: (Num a, Floating a) => Int -> (String, [Int], [Int]) -> Matrix a
gate_to_matrix size (name, [q], []) = moving size sw m where
    q' = q
    sw = []
    m = between (q'-1) (name_to_matrix 1 0 name) (size - q')
gate_to_matrix size (name, [q], [c]) = moving size sw m where
    c' = min q c
    q' = max q c
    sw = (q', c'+1) : (if q < c then [(q, c)] else [])
    m = between (c'-1) (name_to_matrix 1 1 name) (size - q')

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
myfourthcirc :: [Qubit] -> Circ Int
myfourthcirc (q1:_) = do
    hadamard q1
    qnot q1
    hadamard q1
    qnot q1
    return 1

mythirdcirc :: [Qubit] -> Circ Int
mythirdcirc (q1:q2:q3:_) = do
    qnot_at q2 `controlled` q1
    qnot_at q2 `controlled` q3
    qnot_at q2 `controlled` q1
    qnot_at q2 `controlled` q3
    return 3

myothercirc :: [Qubit] -> Circ Int
myothercirc (q1:_) = do
    hadamard q1
    hadamard q1
    return 1

mycirc :: [Qubit] -> Circ Int
mycirc (q1:q2:q3:q4:q5:q6:_) = do
    qnot_at q1 `controlled` q2
    qnot_at q2 `controlled` q3
    qnot_at q5 `controlled` q6
    qnot_at q4 `controlled` q5
    qnot_at q3 `controlled` q4
    return 6

--- Converter ---
to_qmc :: [(Int, Matrix Expr)] -> String
to_qmc sts = "qmc\n"
          ++ "module Test Double\n"
--           const matrix asdf = [1,2;3,4];
--           "mf2so([1,0,0,0; 0,0,0,0; 0,0,0,0; 0,0,0,0])
          ++ concatMap (uncurry matrix_to_qmc) sts
          ++ "s: [0.." ++ show (length sts) ++ "] init 0;\n"
          ++ concatMap (state_to_qmc . fst) sts
          ++ "[] (s = " ++ show (length sts) ++ ") -> (s' = " ++ show (length sts) ++ ")" where
    l = length sts

matrix_to_qmc :: (Show a) => Int -> Matrix a -> String
matrix_to_qmc s m = "const matrix M_" ++ show s ++ " = [" ++ inner ++ "];\n" where
    inner = concat $ intersperse ";" $ map sl $ toLists m
    sl l = concat $ intersperse "," $ map show l

state_to_qmc :: Int -> String
state_to_qmc s = "[] (s = " ++ show s ++ ")"
              ++ " -> "
              ++ "M_" ++ show s
              ++ " : "
              ++ "(s' = " ++ show (s + 1) ++ ")\n"

main = do
    putStr $ strashow $ circ_stratify mycirc
    putStr "---\n"
    print $ circ_matrixes mycirc
    putStr "\n---\n---\n"
    putStr $ strashow $ circ_stratify myothercirc
    putStr "---\n"
    print $ circ_matrixes myothercirc
