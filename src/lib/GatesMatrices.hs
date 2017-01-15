{-# LANGUAGE MultiParamTypeClasses #-}

module GatesMatrices (
    nameToMatrix,
    nameToMatrixParameterized) where

import           Complex
import           Expr
import           QMatrix

-- |nameToMatrix takes a gate name and returns its matrix
nameToMatrix :: (QCMatrix m a, Floating a, Fractional a) => String -> m (Complex a)
nameToMatrix "not"  = pauliX
nameToMatrix "X"    = pauliX
nameToMatrix "Z"    = pauliZ
nameToMatrix "H"    = hadamard
nameToMatrix "W"    = swapSqrt
nameToMatrix "swap" = swap
nameToMatrix n      = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

nameToMatrixParameterized :: (QCMatrix m a, FromDouble a, Floating a) => String -> Double -> m (Complex a)
nameToMatrixParameterized "R(2pi/%)" n = phaseShift (2 * pi / fromDouble n)
nameToMatrixParameterized n       _ = error $ "Parameterized gate \"" ++ show n ++ "\" is not supported yet"
