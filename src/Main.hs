module Main where

import Quipper

import EntangleMonad
import Examples
import QTuple
import Qpmc

-- |fullOut takes a function returning a value in the 'Circ' monad,
-- and outputs the result of transforming it to QPMC code
fullOut :: QTuple a => (a -> Circ b) -> IO ()
fullOut c = do
    putStr "---\n"
    let transitions = circMatrices c
    let destinations = concatMap trDestinations transitions
    let maxBool = foldr (max . length. snBs . fst) 0 destinations
    putStrLn $ toQpmc transitions maxBool 
    putStr "---\n"

main :: IO ()
main =
  --fullOut grover_naive
  --fullOut test_matrix_3
  --fullOut test_matrix_3
  --fullOut strange
  fullOut mycirc
  --fullOut test_if
