{-# LANGUAGE GADTs #-}

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

import Quipper
import Quipper.QData
import Quipper.Monad

-- AST for Circuits
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

-- AST to Circ
eval' :: (Show a) => Circuit a Qubit -> Circ a
eval' (Input x) = return x
eval' (Init b) = qinit b
eval' (QNotControlled w c) = do
    qw <- eval' w
    qc <- eval' c
    qnot qw `controlled` qc
eval' (Hadamard x) = do
    qx <- eval' x
    hadamard qx

eval :: (Show a, Show b) => (Circuit a Qubit -> Circuit b Qubit) -> a -> Circ b
eval f i = eval' $ f (Input i)

-- Example AST
example1 :: Qubit -> Circ Qubit
example1 x = do
    a <- qinit False
    b <- hadamard a
    c <- hadamard x
    d <- qnot c `controlled` b
    e <- hadamard d
    f <- qnot e `controlled` b
    return f

example2 :: Qubit -> Circ Qubit
example2 = eval example_ast

example_ast x = result where
    a = Init False
    b = Hadamard a
    c = Hadamard x
    d = QNotControlled c b
    e = Hadamard d
    f = QNotControlled e d
    result = f

main = do
    print_simple Preview example1
    print_simple Preview example2
