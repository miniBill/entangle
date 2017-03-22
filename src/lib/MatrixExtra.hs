{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MatrixExtra (
    kronDecompose2
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

-- ⎛⎜⎝⎞⎟⎠
--             ⎛a c⎞   ⎛p⎞
-- ⎛a⎞   ⎛c⎞   ⎜a d⎟   ⎜q⎟
-- ⎜ ⎟ x ⎜ ⎟ = ⎜   ⎟ = ⎜ ⎟
-- ⎝b⎠   ⎝d⎠   ⎜b c⎟   ⎜r⎟
--             ⎝b d⎠   ⎝s⎠
-- ac = p
-- ad = q
-- bc = r
-- bd = s
-- |a|² + |b|² = 1
-- |c|² + |d|² = 2

-- a = 0 => |b| = 1
-- bc bd = r s

kronDecompose2 :: (Eq a, Num a) => Matrix a -> Either String (Matrix a, Matrix a)
kronDecompose2 m
    | ncols m /= 1 = Left "kronDecompose2 only supports vectors"
    | nrows m /= 4 = Left "kronDecompose2 only supports vectors with 4 elements for now"
    | otherwise =
        let
            p = m ! (1, 1)
            q = m ! (2, 1)
            r = m ! (3, 1)
            s = m ! (4, 1)
        in
            if p == 0 || q == 0 || r == 0 || s == 0
                then
                    Left "Ambiguous"
                else
                    Right (undefined, undefined)
