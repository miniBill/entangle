{-# LANGUAGE MultiParamTypeClasses #-}

module GatesMatrices (
    nameToMatrix,
    nameToGenR) where

import           Complex
import           MatrixExtra

type MatrixGenR a = Integer -> Integer -> Integer -> a

-- |nameToMatrix takes a gate name and returns its matrix
nameToMatrix :: (GCMatrix m a, Floating a, Fractional a) => String -> m (Complex a)
nameToMatrix "not"  = pauliX
nameToMatrix "X"    = pauliX
nameToMatrix "Z"    = pauliZ
nameToMatrix "H"    = hadamard
nameToMatrix "W"    = swapSqrt
nameToMatrix "swap" = swap
nameToMatrix n      = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

nameToGenR :: Floating a => String -> MatrixGenR a
nameToGenR "rGate" = rotationMatrix
nameToGenR n       = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

-- |rotationMatrix is the matrix for the rGate. NOTE: This version is temporary.
rotationMatrix :: (Num a, Floating a) => MatrixGenR a
rotationMatrix 1 1 _ = 1
rotationMatrix 2 2 n = exp((2*pi*sqrt(-1))/2^n)
