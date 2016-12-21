{-# LANGUAGE MultiParamTypeClasses #-}

module GatesMatrices (
    nameToMatrix,
    nameToGenR) where

import           MatrixExtra

type MatrixGenR a = Integer -> Integer -> Integer -> a

-- |nameToMatrix takes a gate name and returns its matrix
nameToMatrix :: (GMatrix m a, Floating a) => String -> m a
nameToMatrix "not"  = pauliX
nameToMatrix "X"    = pauliX
nameToMatrix "Z"    = pauliZ
nameToMatrix "H"    = hadamard
nameToMatrix "W"    = matrix 4 4 wMatrix
nameToMatrix "swap" = swap
nameToMatrix n      = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

nameToGenR :: Floating a => String -> MatrixGenR a
nameToGenR "rGate" = rotationMatrix
nameToGenR n       = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

-- |rotationMatrix is the matrix for the rGate. NOTE: This version is temporary.
rotationMatrix :: (Num a, Floating a) => MatrixGenR a
rotationMatrix 1 1 _ = 1
rotationMatrix 2 2 n = exp((2*pi*sqrt(-1))/2^n)

-- |wMatrix is the gate_W, the square root of the swap matrix
wMatrix :: (Num a, Floating a) => Integer -> Integer -> a
wMatrix 1 1 = 1
wMatrix 2 2 = 1
wMatrix 2 3 = 1 / sqrt 2
wMatrix 3 2 = 1 / sqrt 2
wMatrix 3 3 = -1 / sqrt 2
wMatrix 4 4 = 1
wMatrix _ _ = 0
