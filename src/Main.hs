module Main where

import Quipper

import Data.List
import Data.Matrix

import EntangleMonad
import Examples
import SqMath
import Stratify
import Tuple

--- Converter ---
-- |to_qmc takes a list of transitions and returns their representation in QPMC code
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

-- |final_to_qmc returns the QPMC code for a final state
final_to_qmc :: Int -> String
final_to_qmc s = "  [] (s = " ++ show s ++ ") -> (s' = " ++ show s ++ ");\n"

-- |matrix_to_qmc returns the QPMC code for a matrix
matrix_to_qmc :: Show a => (Matrix a, Int) -> String
matrix_to_qmc (m, t) = "const matrix A" ++ show t ++ " = [" ++ inner ++ "];\n" where
    inner = intercalate ";" $ map sl $ toLists m
    sl l = intercalate "," $ map show l

-- |transition_to_qmc returns the QPMC code for a transition
transition_to_qmc :: Transitions a -> String
transition_to_qmc (f, ts) = "  [] (s = " ++ show f ++ ")"
              ++ " -> "
              ++ intercalate " + " (map transition_to_qmc' ts)
              ++ ";\n"

-- |transition_to_qmc' is an helper function used by 'transition_to_qmc'
transition_to_qmc' :: (Matrix a, Int) -> String
transition_to_qmc' (_, t) = "<<A" ++ show t
              ++ ">> : "
              ++ "(s' = " ++ show t ++ ")"

-- |full_out takes a function returning a value in the 'Circ' monad,
-- and outputs the result of transforming it to QPMC code
full_out :: Tuple a => (a -> Circ b) -> IO ()
full_out c = do
--    putStr "---\n"
--    print $ circ_matrices c
    putStr "---\n"
    putStr $ strashow $ circ_stratify c
    putStr "---\n"
    putStrLn $ to_qmc $ circ_matrices c
    putStr "---\n"

full_out_rec :: Tuple a => (a -> Circ Bool) -> IO ()
full_out_rec c = do
    putStr "---\n"
    print $ circ_matrices c
    putStr "---\n"

main = do
  --full_out grover_naive
  --full_out test_matrix_3
  --full_out test_matrix_3
  --full_out strange
  full_out mycirc
  --full_out test_if

