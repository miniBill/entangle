module Test where

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
matrix_multiply :: Matrix -> Matrix -> Matrix
matrix_multiply = undefined

circ_matrixes :: TransCirc -> [(Int, Matrix)]
circ_matrixes circ = map (\(s, gs) -> (s, f gs)) stratified where
    stratified = circ_stratify circ
    size = 2 ^ qubit_max
    gate_list = concatMap snd stratified
    qubit_max = maximum $ concatMap (\(_, ms, cs) -> ms ++ cs) gate_list
    f gs = foldr (\g m -> gate_to_matrix g * m) (identity size) gs
    gate_to_matrix

--- Esempio ---

test = [[1, 2],
        [2, 3],
        [5, 6],
        [4, 5],
        [3, 4]]

mycirc :: [Qubit] -> Circ Int
mycirc (q1:q2:q3:q4:q5:q6:_) = do
    qnot_at q1 `controlled` q2
    qnot_at q2 `controlled` q3
    --qnot_at q5 `controlled` q6
    --qnot_at q4 `controlled` q5
    --qnot_at q3 `controlled` q4
    return 3

output = putStr $ strashow $ circ_stratify mycirc
