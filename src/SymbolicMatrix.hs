{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SymbolicMatrix (
    SymbolicMatrix
    ) where

import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))

import           Complex
import           QMatrix
import           Qpmc

data StandardMatrix a
    = Identity Integer
    | Hadamard
    | PauliX
    | PauliZ
    | ControlNot
    | Swap
    | PhaseShift a
    | Measure MeasureKind

data SymbolicMatrix a
    = StandardMatrix (StandardMatrix a)
    | Zero Integer Integer
    | Matrix Integer Integer (Integer -> Integer -> a)
    | Multiply (SymbolicMatrix a) (SymbolicMatrix a)
    | Kronecker (SymbolicMatrix a) (SymbolicMatrix a)
    | HorizontalJoin (SymbolicMatrix a) (SymbolicMatrix a)
    | VerticalJoin (SymbolicMatrix a) (SymbolicMatrix a)

instance Show a => Show (StandardMatrix a) where
    show (Identity i)   = "identity(" ++ show i ++ ")"
    show Hadamard       = "Hadamard"
    show PauliX         = "PauliX"
    show PauliZ         = "PauliZ"
    show ControlNot     = "CNOT"
    show Swap           = "Swap"
    show (PhaseShift d) = "PhaseShift(" ++ show d ++ ")"
    show (Measure UL)   = "M0"
    show (Measure BR)   = "M1"

instance Show a => Show (SymbolicMatrix a) where
    show (StandardMatrix m) = show m
    show (Zero r c) = "?Zero " ++ show r ++ " " ++ show c
    show (Matrix 2 2 f) = "[" ++ show (f 1 1) ++ ", " ++ show (f 1 2) ++ "; " ++ show (f 2 1) ++ ", " ++ show (f 2 2) ++ "]"
    show (Matrix r c _)  = "?Matrix " ++ show r ++ " " ++ show c
    show (Kronecker a b) = "kron (" ++ show a ++ ", " ++ show b ++ ")"
    show (Multiply a b)  = "?Multiply (" ++ show a ++ ") (" ++ show b ++ ")"
    show (HorizontalJoin l r) = "?HorizontalJoin (" ++ show l ++ ") (" ++ show r ++ ")"
    show (VerticalJoin u d) = "?VerticalJoin (" ++ show u ++ ") (" ++ show d ++ ")"

instance Num (SymbolicMatrix a) where
    (*) (StandardMatrix (Identity _)) b = b
    (*) a (StandardMatrix (Identity _)) = a
    (*) a b                             = Multiply a b

instance (Num a) => QMatrix SymbolicMatrix a where
    kronecker a (StandardMatrix (Identity 1)) = a
    kronecker (StandardMatrix (Identity 1)) b = b
    kronecker a b                             = Kronecker a b

    identity = StandardMatrix . Identity

    zero = Zero

    matrix = Matrix
    (<->) = VerticalJoin
    (<|>) = HorizontalJoin

    hadamard = StandardMatrix Hadamard
    pauliX = StandardMatrix PauliX
    pauliZ = StandardMatrix PauliZ
    swap = StandardMatrix Swap
    measure = StandardMatrix . Measure

instance (Floating a, Fractional a, Show a, Num a, Eq a) => QCMatrix SymbolicMatrix a where
    phaseShift t = StandardMatrix $ PhaseShift $ t :+ 0

instance Show a => ToQpmc (SymbolicMatrix a) where
    toQpmc = show
