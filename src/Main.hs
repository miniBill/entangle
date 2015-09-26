{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Debug.Trace

import qualified Quipper as Q
import qualified Quipper.QData as QD

class (Monad m) => QMonad m q where
    hadamard :: q -> m q
    qnot_at :: q -> m ()
    controlled :: m a -> [q] -> m a
    with_ancilla :: (q -> m a) -> m a

class (Monad m) => QControl m where
    with_controls :: Q.ControlList -> m a -> m a

class QCheck q where
    (.==.) :: q -> Bool -> Q.ControlList
    infix 4 .==.

(.&&.) :: Q.ControlList -> Q.ControlList -> Q.ControlList
(.&&.) = (Q..&&.)
infixr 3 .&&.

(..==..) :: Q.Qubit -> Bool -> Q.ControlList
q ..==.. i = (Q..==.) q i

instance QMonad Q.Circ Q.Qubit where
    hadamard = Q.hadamard
    qnot_at = Q.qnot_at
    controlled = Q.controlled
    with_ancilla = Q.with_ancilla

instance QControl Q.Circ where
    with_controls = Q.with_controls

instance QCheck Q.Qubit where
    (.==.) = (..==..)

-- AST for muits
data Circuit a q where
    Input :: a -> Circuit a q
    Init :: Bool -> Circuit q q
    QNotControlled :: Circuit q q -> Circuit q q -> Circuit q q
    Hadamard :: Circuit q q -> Circuit q q

-- Show Instance
instance (Show a, Show q) => Show (Circuit a q) where
    show x = showi 0 x

showi :: (Show a, Show q) => Int -> Circuit a q -> String
showi i (Input x) = indent i ++ "Input " ++ show x
showi i (Init x) = indent i ++ "Init " ++ show x
showi i (QNotControlled b c) = indent i ++ "QNotControlled\n" ++ showi (i+1) b ++ "\n" ++ showi (i+1) c
showi i (Hadamard x) = indent i ++ "Hadamard\n" ++ showi (i+1) x

indent :: (Num a, Eq a) => a -> String
indent 0 = "|"
indent i = "|    " ++ indent (i-1)


-- This file is part of Q. Copyright (C) 2011-2014. Please see the
-- file COPYRIGHT for a list of authors, copyright holders, licensing,
-- and other details. All rights reserved.
--
-- ======================================================================

circuit :: (QMonad m q) => q -> q -> q -> m (q, q, q)
circuit a b c = do
  qnot_at a `controlled` [b]
  qnot_at b `controlled` [c]
  hadamard c `controlled` [a,b]
  return (a, b, c)

hadamard2 :: (QMonad m q) => q -> q -> q -> m (q, q, q)
hadamard2 h a b = do
  with_ancilla (f h a b)
  return (h, a, b) where

f :: (QMonad m q) => q -> q -> q -> q -> m ()
f h a b c = do
  qnot_at c `controlled` [a, b]
  hadamard h `controlled` [c]
  qnot_at c `controlled` [a, b]

example :: (QMonad m q, QControl m, QCheck q) => q -> q -> q -> q -> q -> m (q, q, q, q, q)
example a b c d e = do
  circuit a b c
  circuit b c a
  with_controls (d .==. 1 .&&. e .==. 0) $ do
      circuit a b c
      circuit b c a
  circuit a b c
  circuit b c a
  return (a, b, c, d, e)

example' :: Q.Qubit -> Q.Qubit -> Q.Qubit -> Q.Qubit -> Q.Qubit ->
    Q.Circ (Q.Qubit, Q.Qubit, Q.Qubit, Q.Qubit, Q.Qubit)
example' = example

main = Q.print_simple Q.Preview example'
