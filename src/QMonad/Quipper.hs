{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  QMonad.Quipper
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

module QMonad.Quipper (

) where

import QMonad

import qualified Quipper as Q
import qualified Quipper.QData as QD

(..==..) :: Qubit Q.Circ -> Bool -> ControlList Q.Circ
(QQubit q) ..==.. i = QControlList $ (Q..==.) q i

(..&&..) :: ControlList Q.Circ -> ControlList Q.Circ -> ControlList Q.Circ
(QControlList l) ..&&.. (QControlList r) = QControlList $ l Q..&&. r

instance QMonad Q.Circ where
    data Qubit Q.Circ = QQubit Q.Qubit
    data ControlList Q.Circ = QControlList Q.ControlList
    type EQubit Q.Circ = Q.Qubit
    embed = QQubit
    umbed (QQubit q) = q
    hadamard (QQubit q) = fmap QQubit $ Q.hadamard q
    qnot (QQubit q) = fmap QQubit $ Q.qnot q
    controlled c ls = Q.controlled c [q | (QQubit q) <- ls]
    with_ancilla f = Q.with_ancilla (\q -> f $ QQubit q)
    with_controls (QControlList cl) = Q.with_controls cl
    (.==.) = (..==..)
    (.&&.) = (..&&..)
