module GatesMatrices (
    nameToGen,
    swapMatrix) where

type MatrixGen a = Int -> Int -> a

-- |nameToGen takes a gate name and returns its matrix, as a function
nameToGen :: Floating a => String -> MatrixGen a
nameToGen "not" = notMatrix
nameToGen "Z" = zMatrix
nameToGen "X" = notMatrix
nameToGen "H" = hadamardMatrix
nameToGen "W" = wMatrix
nameToGen "swap" = swapMatrix
nameToGen n = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

-- |hadamardMatrix is the matrix for the Hadamard gate
hadamardMatrix :: (Num a, Floating a) => MatrixGen a
hadamardMatrix 2 2 = -1 / sqrt 2
hadamardMatrix _ _ = 1 / sqrt 2

-- |notMatrix is the matrix for the Not gate
notMatrix :: Num a => MatrixGen a
notMatrix 1 2 = 1
notMatrix 2 1 = 1
notMatrix _ _ = 0

-- |zMatrix is the matrix for the Z gate
zMatrix :: Num a => MatrixGen a
zMatrix 1 1 = 1
zMatrix 2 2 = -1
zMatrix _ _ = 0

-- |swapMatrix is a matrix that swaps two qubits
swapMatrix :: Num a => MatrixGen a
swapMatrix 1 1 = 1
swapMatrix 2 3 = 1
swapMatrix 3 2 = 1
swapMatrix 4 4 = 1
swapMatrix _ _ = 0

-- |wMatrix is the gate_W, the square root of the swap matrix
wMatrix :: (Num a, Floating a) => MatrixGen a
wMatrix 1 1 = 1
wMatrix 2 2 = 1
wMatrix 2 3 = 1 / sqrt 2
wMatrix 3 2 = 1 / sqrt 2
wMatrix 3 3 = -1 / sqrt 2
wMatrix 4 4 = 1
wMatrix _ _ = 0
