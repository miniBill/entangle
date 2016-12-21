{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import           Quipper

import           Data.Matrix (Matrix)

import           Examples
import           MatrixExtra
import           Qpmc
import           QTuple
import           SqMath
import           Transitions

-- |fullOut takes a function returning a value in the 'Circ' monad,
-- and outputs the result of transforming it to QPMC code
--fullOut :: QTuple a => (a -> Circ b) -> IO ()
fullOut :: (QTuple a, Show b, GCMatrix m Expr) => m x -> (b -> [Transition m Expr]) -> (a -> Circ b) -> IO ()
fullOut _ final c = do
    putStr "---\n"
    let tree = circToTree c
    print tree
    putStr "---\n"
    let transitions = circMatrices final c
    putStrLn $ toQpmc transitions
    putStr "---\n"

nonrecursive :: a -> [Transition m Expr]
nonrecursive = const []

recursive :: RecAction -> [Transition m v]
recursive Exit = []
recursive Loop = [Transition Nothing $ StateName 0 []]

symbolic :: SymbolicMatrix a
symbolic = error "proxy"

numeric :: Matrix a
numeric = error "proxy"

main :: IO ()
main = fullOut
  symbolic
  --numeric

  --nonrecursive grover_naive
  --nonrecursive test_matrix_3
  --nonrecursive test_matrix_3
  --nonrecursive strange
  --nonrecursive mycirc
  --nonrecursive test_if
  --recursive recCirc'
  --recursive branchCirc
  recursive groverRec
