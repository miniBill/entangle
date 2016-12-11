{-# LANGUAGE RankNTypes #-}

module Main where

import           Quipper

import           Examples
import           Qpmc
import           QTuple
import           SqMath
import           Transitions

-- |fullOut takes a function returning a value in the 'Circ' monad,
-- and outputs the result of transforming it to QPMC code
--fullOut :: QTuple a => (a -> Circ b) -> IO ()
fullOut :: (QTuple a, Show b) => (b -> [Transition Expr]) -> (a -> Circ b) -> IO ()
fullOut final c = do
    putStr "---\n"
    let tree = circToTree c
    print tree
    putStr "---\n"
    let transitions = circMatrices final c
    putStrLn $ toQpmc transitions
    putStr "---\n"

nonrecursive :: a -> [Transition Expr]
nonrecursive = const []

recursive :: RecAction -> [Transition v]
recursive Exit = []
recursive Loop = [Transition Nothing $ StateName 0 []]

main :: IO ()
main = fullOut
  --nonrecursive grover_naive
  --nonrecursive test_matrix_3
  --nonrecursive test_matrix_3
  --nonrecursive strange
  --nonrecursive mycirc
  --nonrecursive test_if
  --recursive recCirc'
  --recursive branchCirc
  recursive groverRec
