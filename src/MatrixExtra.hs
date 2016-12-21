{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MatrixExtra (
    kronecker, identity, matrix, zero, matrixToQpmc, hadamard,
    pauliX, pauliZ, swap, swapSqrt, phaseShift,
    (<->), (<|>),
    GMatrix, GCMatrix,

    SymbolicMatrix
    ) where

import           Data.List
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix

import           Complex

data StandardMatrix a
    = Identity Integer
    | Hadamard
    | PauliX
    | PauliZ
    | ControlNot
    | Swap
    | PhaseShift a

data SymbolicMatrix a
    = StandardMatrix (StandardMatrix a)
    | Zero Integer Integer
    | Matrix Integer Integer (Integer -> Integer -> a)
    | Multiply (SymbolicMatrix a) (SymbolicMatrix a)
    | Kronecker (SymbolicMatrix a) (SymbolicMatrix a)
    | HorizontalJoin (SymbolicMatrix a) (SymbolicMatrix a)
    | VerticalJoin (SymbolicMatrix a) (SymbolicMatrix a)

instance Show a => Show (StandardMatrix a) where
    show (Identity i) = "ID(" ++ show i ++ ")"
    show Hadamard = "HD"
    show PauliX = "PX"
    show PauliZ = "PZ"
    show ControlNot = "CN"
    show Swap = "SW"
    show (PhaseShift d) = "PS(" ++ show d ++ ")"

instance Show a => Show (SymbolicMatrix a) where
    show (StandardMatrix m) = show m
    show (Zero r c) = "Zero " ++ show r ++ " " ++ show c
    show (Matrix r c _)  = "Matrix " ++ show r ++ " " ++ show c
    show (Kronecker a b) = "Kronecker (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Multiply a b)  = "Multiply (" ++ show a ++ ") (" ++ show b ++ ")"
    show (HorizontalJoin l r) = "HorizontalJoin (" ++ show l ++ ") (" ++ show r ++ ")"
    show (VerticalJoin u d) = "VerticalJoin (" ++ show u ++ ") (" ++ show d ++ ")"

class (Num a, Num (m a)) => GMatrix m a where
    -- |kronecker is the Kronecker product
    kronecker :: m a -> m a -> m a
    identity :: Integer -> m a
    zero :: Integer -> Integer -> m a
    matrix :: Integer -> Integer -> (Integer -> Integer -> a) -> m a
    matrixToQpmc :: m a -> String
    (<->) :: m a -> m a -> m a
    (<|>) :: m a -> m a -> m a

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

class (Fractional a, Floating a, GMatrix m (Complex a)) => GCMatrix m a where
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

    phaseShift :: a -> m (Complex a)
    phaseShift phi = matrix 2 2 phaseShiftMatrix where
        phaseShiftMatrix 1 1 = 1
        phaseShiftMatrix 2 2 = exp $ ii * (phi :+ 0)
        phaseShiftMatrix _ _ = 0

instance Num (SymbolicMatrix a) where
    (*) = Multiply

instance (Show a, Num a) => GMatrix SymbolicMatrix a where
    kronecker a (StandardMatrix (Identity 1)) = a
    kronecker (StandardMatrix (Identity 1)) b = b
    kronecker a b            = Kronecker a b

    identity = StandardMatrix . Identity

    zero = Zero

    matrix = Matrix
    matrixToQpmc = show
    (<->) = VerticalJoin
    (<|>) = HorizontalJoin

    hadamard = StandardMatrix Hadamard
    pauliX = StandardMatrix PauliX
    pauliZ = StandardMatrix PauliZ
    swap = StandardMatrix Swap

instance (Floating a, Fractional a, Show a) => GCMatrix SymbolicMatrix a where
    phaseShift t = StandardMatrix $ PhaseShift $ t :+ 0

downcast :: Integer -> Int
downcast x
    | x > fromIntegral (maxBound :: Int) = error "Overflow!"
    | otherwise = fromIntegral x

instance (Num a, Show a) => GMatrix Matrix a where
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

    matrixToQpmc mat =
        let
            sl l = intercalate "," $ map show l
            inner = intercalate ";" $ map sl $ toLists mat
        in
            "[" ++ inner ++ "]"

    (<->) = (Data.Matrix.<->)

    (<|>) = (Data.Matrix.<|>)
