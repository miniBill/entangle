{-# LANGUAGE MultiParamTypeClasses #-}

module GatesMatrices (
    nameToMatrix,
    nameToParameterizedMatrix) where

import           Complex
import           Expr
import           MatrixExtra

-- |nameToMatrix takes a gate name and returns its matrix
nameToMatrix :: (GCMatrix m a, Floating a, Fractional a) => String -> m (Complex a)
nameToMatrix "not"  = pauliX
nameToMatrix "X"    = pauliX
nameToMatrix "Z"    = pauliZ
nameToMatrix "H"    = hadamard
nameToMatrix "W"    = swapSqrt
nameToMatrix "swap" = swap
nameToMatrix n      = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

nameToParameterizedMatrix :: (GCMatrix m a, FromDouble a, Floating a) => String -> Double -> m (Complex a)
nameToParameterizedMatrix "R(2pi/%)" n = phaseShift (2 * pi / fromDouble n)
nameToParameterizedMatrix n       _ = error $ "Parameterized gate \"" ++ show n ++ "\" is not supported yet"
