{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MatrixExtra (
    eval, evalN
    ) where

import           Data.List
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix

import           Expr
import           QMatrix
import           Qpmc

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

instance Show a => ToQpmc (Matrix a) where
    toQpmc mat =
        let
            sl l = intercalate "," $ map show l
            inner = intercalate ";" $ map sl $ toLists mat
        in
            "[" ++ inner ++ "]"

evalN :: SymbolicMatrix Expr -> Matrix Expr
evalN = eval

eval :: (Floating a, Num a, Show a, QMatrix m a) => SymbolicMatrix a -> m a
eval (Zero r c) = zero r c
eval (Matrix r c f) = matrix r c f
eval (Kronecker a b) = kronecker (eval a) (eval b)
eval (Multiply a b) = eval a * eval b
eval (HorizontalJoin a b) = eval a <|> eval b
eval (VerticalJoin a b) = eval a <-> eval b
eval (StandardMatrix m) = eval' m where
    eval' (Identity i) = identity i
    eval' Hadamard     = hadamard
    eval' PauliX       = pauliX
    eval' PauliZ       = pauliZ
    eval' Swap         = swap
    --eval' (PhaseShift d) = phaseShift d
    eval' (Measure k)  = measure k
