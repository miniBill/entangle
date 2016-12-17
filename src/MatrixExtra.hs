{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MatrixExtra (
    kronecker, identity, matrix, zero, matrixToQpmc,
    (<->), (<|>),
    GMatrix,

    SymbolicMatrix
    ) where

import           Data.List
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix

data SymbolicMatrix a
    = Identity Integer
    | Zero Integer Integer
    | Matrix Integer Integer (Integer -> Integer -> a)
    | Multiply (SymbolicMatrix a) (SymbolicMatrix a)
    | Kronecker (SymbolicMatrix a) (SymbolicMatrix a)
    | HorizontalJoin (SymbolicMatrix a) (SymbolicMatrix a)
    | VerticalJoin (SymbolicMatrix a) (SymbolicMatrix a)

instance Show (SymbolicMatrix a) where
    show (Identity i)    = "Identity " ++ show i
    show (Zero r c) = "Zero " ++ show r ++ " " ++ show c
    show (Kronecker a b) = "Kronecker (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Matrix r c _)  = "Matrix " ++ show r ++ " " ++ show c
    show (Multiply a b)  = "Multiply (" ++ show a ++ ") (" ++ show b ++ ")"
    show (HorizontalJoin l r) = "HorizontalJoin (" ++ show l ++ ") (" ++ show r ++ ")"
    show (VerticalJoin u d) = "VerticalJoin (" ++ show u ++ ") (" ++ show d ++ ")"

class Num (m a) => GMatrix m a where
    -- |kronecker is the Kronecker product
    kronecker :: m a -> m a -> m a
    identity :: Integer -> m a
    zero :: Integer -> Integer -> m a
    matrix :: Integer -> Integer -> (Integer -> Integer -> a) -> m a
    matrixToQpmc :: m a -> String
    (<->) :: m a -> m a -> m a
    (<|>) :: m a -> m a -> m a

instance Num (SymbolicMatrix a) where
    (*) = Multiply

instance GMatrix SymbolicMatrix a where
    kronecker a (Identity 1) = a
    kronecker (Identity 1) b = b
    kronecker a b            = Kronecker a b

    identity = Identity

    zero = Zero

    matrix = Matrix
    matrixToQpmc = show
    (<->) = VerticalJoin
    (<|>) = HorizontalJoin

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
