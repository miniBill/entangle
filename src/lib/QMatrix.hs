{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QMatrix (
    MeasureKind(..),

    QMatrix, QCMatrix,
    kronecker, identity, matrix, zero, hadamard,
    pauliX, pauliZ, pauliY,swap, swapSqrt, phaseShift, measure,
    (<->), (<|>)
    ) where

import           Complex
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix

data MeasureKind = UL | BR

class (Num a, Num (m a)) => QMatrix m a where
    -- |kronecker is the Kronecker product
    kronecker :: m a -> m a -> m a
    matrix :: Integer -> Integer -> (Integer -> Integer -> a) -> m a
    (<->) :: m a -> m a -> m a
    (<|>) :: m a -> m a -> m a

    zero :: Integer -> Integer -> m a
    zero r c = matrix r c $ \_ _ -> 0

    identity :: Integer -> m a
    identity n = matrix n n $ \r c -> if r == c then 1 else 0

    -- |hadamard is the matrix for the Hadamard gate
    hadamard :: Floating a => m a
    hadamard = matrix 2 2 hadamardMatrix where
        hadamardMatrix 2 2 = -1 / sqrt 2
        hadamardMatrix _ _ = 1 / sqrt 2

    -- |pauliX is the Pauli X matrix (Not)
    pauliX :: m a
    pauliX = matrix 2 2 pauliXMatrix where
        pauliXMatrix 1 2 = 1
        pauliXMatrix 2 1 = 1
        pauliXMatrix _ _ = 0


    -- |pauliZ is the Pauli Z matrix
    pauliZ :: m a
    pauliZ = matrix 2 2 pauliZMatrix where
        pauliZMatrix 1 1 = 1
        pauliZMatrix 2 2 = -1
        pauliZMatrix _ _ = 0

    -- |swap is a matrix that swaps two qubits
    swap :: m a
    swap = matrix 4 4 swapMatrix where
        swapMatrix 1 1 = 1
        swapMatrix 2 3 = 1
        swapMatrix 3 2 = 1
        swapMatrix 4 4 = 1
        swapMatrix _ _ = 0

    -- |measure is the measure matrix
    -- measure UL is [1, 0; 0, 0] whereas measure BR is [0, 0; 0, 1]
    measure :: MeasureKind -> m a
    measure k =
        let
            gen UL 1 1 = 1
            gen BR 2 2 = 1
            gen _ _ _  = 0
        in
            matrix 2 2 (gen k)


class (Fractional a, Floating a, QMatrix m (Complex a)) => QCMatrix m a where
    -- |swapSqrt is the gate_W, the square root of the swap matrix
    swapSqrt :: m (Complex a)
    swapSqrt = matrix 4 4 swapSqrtMatrix where
        swapSqrtMatrix 1 1 = 1
        swapSqrtMatrix 2 2 = (1 + ii) / 2
        swapSqrtMatrix 2 3 = (1 - ii) / 2
        swapSqrtMatrix 3 2 = (1 - ii) / 2
        swapSqrtMatrix 3 3 = (1 + ii) / 2
        swapSqrtMatrix 4 4 = 1
        swapSqrtMatrix _ _ = 0

    -- |pauliY is the Pauli Y matrix 
    pauliY :: m (Complex a)
    pauliY = matrix 2 2 pauliYMatrix where
        pauliYMatrix 1 2 = -ii
        pauliYMatrix 2 1 = ii
        pauliYMatrix _ _ = 0

    phaseShift :: a -> m (Complex a)
    phaseShift phi = matrix 2 2 phaseShiftMatrix where
        phaseShiftMatrix 1 1 = 1
        phaseShiftMatrix 2 2 = exp $ ii * (phi :+ 0)
        phaseShiftMatrix _ _ = 0

downcast :: Integer -> Int
downcast x
    | x > fromIntegral (maxBound :: Int) = error "Overflow!"
    | otherwise = fromIntegral x

instance Num a => QMatrix Matrix a where
    kronecker a b =
        let
            ra = nrows a
            rb = nrows b
            ca = ncols a
            cb = ncols b
            gen (r, c) = ae * be where
                ae = a ! (ar, ac)
                ar = 1 + (r - 1) `div` rb
                ac = 1 + (c - 1) `div` cb
                be = b ! (br, bc)
                br = 1 + (r - 1) `mod` rb
                bc = 1 + (c - 1) `mod` cb
        in
            Data.Matrix.matrix (ra * rb) (ca * cb) gen

    identity = Data.Matrix.identity . downcast

    zero r c = Data.Matrix.zero (downcast r) (downcast c)

    matrix r c f = Data.Matrix.matrix (downcast r) (downcast c) (\(y, x) -> f (fromIntegral y) (fromIntegral x))

    (<->) = (Data.Matrix.<->)

    (<|>) = (Data.Matrix.<|>)

instance (Fractional a, Floating a) => QCMatrix Matrix a where
