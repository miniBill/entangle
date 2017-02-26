{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MatrixExtra (
    ) where

import           Data.List
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix

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

instance (Fractional a, Floating a) => QCMatrix Matrix a where

instance Show a => ToQpmc (Matrix a) where
    toQpmc mat =
        let
            sl l = intercalate "," $ map show l
            inner = intercalate ";" $ map sl $ toLists mat
        in
            "[" ++ inner ++ "]"
