{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MatrixExtra (
    kronecker, identity, matrix, matrixToQpmc,
    GMatrix,

    SymbolicMatrix
    ) where

import           Data.List
import           Data.Matrix hiding (identity, matrix)
import qualified Data.Matrix

data SymbolicMatrix a
    = Identity Int
    | Matrix Int Int ((Int, Int) -> a)
    | Multiply (SymbolicMatrix a) (SymbolicMatrix a)
    | Kronecker (SymbolicMatrix a) (SymbolicMatrix a)

instance Show (SymbolicMatrix a) where
    show (Identity i) = "Identity " ++ show i
    show (Kronecker a b) = "Kronecker (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Matrix r c _) = "Matrix " ++ show r ++ " " ++ show c
    show (Multiply a b) = "Multiply (" ++ show a ++ ") (" ++ show b ++ ")"

class Num (m a) => GMatrix m a where
    -- |kronecker is the Kronecker product
    kronecker :: m a -> m a -> m a
    identity :: Int -> m a
    matrix :: Int -> Int -> ((Int, Int) -> a) -> m a
    matrixToQpmc :: m a -> String

instance Num (SymbolicMatrix a) where
    (*) = Multiply

instance GMatrix SymbolicMatrix a where
    kronecker = Kronecker
    identity = Identity
    matrix = Matrix
    matrixToQpmc = show

instance (Num a, Show a) => GMatrix Matrix a where
    kronecker a b =
        let
            ra = nrows a
            rb = nrows b
            ca = ncols a
            cb = ncols b
            gen r c = ae * be where
                ae = a ! (ar, ac)
                ar = 1 + (r - 1) `div` rb
                ac = 1 + (c - 1) `div` cb
                be = b ! (br, bc)
                br = 1 + (r - 1) `mod` rb
                bc = 1 + (c - 1) `mod` cb
        in
            matrix (ra * rb) (ca * cb) (uncurry gen)

    identity = Data.Matrix.identity

    matrix = Data.Matrix.matrix

    matrixToQpmc mat =
        let
            sl l = intercalate "," $ map show l
            inner = intercalate ";" $ map sl $ toLists mat
        in
            "[" ++ inner ++ "]"
