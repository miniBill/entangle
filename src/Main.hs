module Main where

import Quipper
import Quipper.Circuit
import Quipper.Monad
import Quipper.Transformer
import Debug.Trace

import Control.Monad.Writer.Lazy
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

circ_matrixes :: Num a => TransCirc -> [(Int, Matrix a)]
circ_matrixes circ = map (\(s, gs) -> (s, f gs)) stratified where
    stratified = circ_stratify circ
    size = 2 ^ qubit_max
    gate_list = concatMap snd stratified
    qubit_max = maximum $ concatMap (\(_, ms, cs) -> ms ++ cs) gate_list
    f gs = foldr (\g m -> gate_to_matrix qubit_max g * m) (identity size) gs

gate_to_matrix :: Num a => Int -> (String, [Int], [Int]) -> Matrix a
gate_to_matrix size ("not", [q], [c]) = moving size sw m where
    sw = (if q < c then [(q,c)] else []) ++ [(q, c+1)]
    m = (identity $ 2 ^ (c-1)) `kronecker` cnot_matrix `kronecker` (identity $ 2 ^ (size - (q-1)))

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
    back  = move size $ reverse $ moves

move :: Num a => Int -> [(Int, Int)] -> Matrix a
move size ms = foldr f (identity (2 ^ size)) ms where
    f (t1, t2) m = swap_to_matrix size t1 t2 * m

swap_to_matrix :: Num a => Int -> Int -> Int -> Matrix a
swap_to_matrix size n m | n > m = swap_to_matrix size m n
                        | n == m = identity (2 ^ size)
                        | n < m - 1 = swap_to_matrix size n (m - 1) * swap_to_matrix size (m - 1) m
                        -- otherwise: n == m - 1
                        | otherwise = before `kronecker` swap_matrix `kronecker` after where
                            before = identity $ 2 ^ (n - 1)
                            after  = identity $ 2 ^ (size - m)

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
myothercirc :: [Qubit] -> Circ Int
myothercirc (q1:q2:q3:q4:_) = do
    qnot_at q1 `controlled` q2
    qnot_at q3 `controlled` q4
    qnot_at q1 `controlled` q2
    qnot_at q3 `controlled` q4
    return 4

mycirc :: [Qubit] -> Circ Int
mycirc (q1:q2:q3:q4:q5:q6:_) = do
    qnot_at q1 `controlled` q2
    qnot_at q2 `controlled` q3
    qnot_at q5 `controlled` q6
    qnot_at q4 `controlled` q5
    qnot_at q3 `controlled` q4
    return 6

main = putStr $ strashow $ circ_stratify mycirc
