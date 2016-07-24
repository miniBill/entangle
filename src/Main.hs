module Main where

import Quipper

import Examples
import QTuple
import Qpmc
import Transitions

-- |fullOut takes a function returning a value in the 'Circ' monad,
-- and outputs the result of transforming it to QPMC code
--fullOut :: QTuple a => (a -> Circ b) -> IO ()
fullOut :: (QTuple a, Show b) => (a -> Circ b) -> IO ()
fullOut c = do
    putStr "---\n"
    let tree = circToTree c
    print tree
    let transitions = circMatrices c
    putStrLn $ toQpmc transitions
    putStr "---\n"

main :: IO ()
main = fullOut
  --grover_naive
  --test_matrix_3
  --test_matrix_3
  --strange
  --mycirc
  --test_if
  --recCirc'
  branchCirc
