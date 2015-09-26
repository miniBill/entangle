{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

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

import QMonad
import QMonad.Quipper
import QMonad.MC

import qualified Quipper as Q

-- This file is part of Q. Copyright (C) 2011-2014. Please see the
-- file COPYRIGHT for a list of authors, copyright holders, licensing,
-- and other details. All rights reserved.
--
-- ======================================================================

circuit :: (QMonad m) => Qubit m -> Qubit m -> Qubit m -> m (Qubit m, Qubit m, Qubit m)
circuit a b c = do
  qnot a `controlled` [b]
  qnot b `controlled` [c]
  hadamard c `controlled` [a,b]
  return (a, b, c)

hadamard2 :: QMonad m => Qubit m -> Qubit m -> Qubit m -> m (Qubit m, Qubit m, Qubit m)
hadamard2 h a b = do
  with_ancilla $ \c -> do
    qnot c `controlled` [a, b]
    hadamard h `controlled` [c]
    qnot c `controlled` [a, b]
    return ()
  return (h, a, b) where

example :: QMonad m => Qubit m -> Qubit m -> Qubit m -> Qubit m -> Qubit m ->
    m (Qubit m, Qubit m, Qubit m, Qubit m, Qubit m)
example a b c d e = do
  circuit a b c
  circuit b c a
  with_controls (d .==. 1 .&&. e .==. 0) $ do
      circuit a b c
      circuit b c a
  circuit a b c
  circuit b c a
  return (a, b, c, d, e)

simple :: QMonad m => Qubit m -> m (Qubit m)
simple q = do
    qnot q
    hadamard q
    qnot q

-- Up to here, original code with generalized types, now we lift it into our two monads
type QQ = Q.Qubit
type QM = Qubit MC

simple' :: QQ -> Q.Circ QQ
simple' = lift1 simple

simple'' :: QM -> MC QM
simple'' = lift1 simple

example' :: QQ -> QQ -> QQ -> QQ -> QQ -> Q.Circ (QQ, QQ, QQ, QQ, QQ)
example' = lift5 example

example'' :: QM -> QM -> QM -> QM -> QM -> MC (QM, QM, QM, QM, QM)
example'' = lift5 example

main = do
    print $ simple (sc 1) -- example'' (sc 0) (sc 1) (sc 2) (sc 3) (sc 4)
    Q.print_simple Q.Preview simple'
