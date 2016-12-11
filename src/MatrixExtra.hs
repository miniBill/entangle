module MatrixExtra (
    kronecker
    ) where

import           Data.Matrix

-- |kronecker is the Kronecker product
kronecker :: Num a => Matrix a -> Matrix a -> Matrix a
kronecker a b = matrix (ra * rb) (ca * cb) (uncurry gen) where
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
