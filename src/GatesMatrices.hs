module GatesMatrices where

name_to_gen "not" = not_matrix
name_to_gen "Z" = z_matrix
name_to_gen "X" = not_matrix
name_to_gen "H" = hadamard_matrix
name_to_gen "W" = w_matrix
name_to_gen "swap" = swap_matrix

-- |hadamard_matrix is the matrix for the Hadamard gate
hadamard_matrix :: (Num a, Floating a) => Int -> Int -> a
hadamard_matrix 2 2 = -1 / sqrt 2
hadamard_matrix _ _ = 1 / sqrt 2

-- |not_matrix is the matrix for the Not gate
not_matrix :: Num a => Int -> Int -> a
not_matrix 1 2 = 1
not_matrix 2 1 = 1
not_matrix _ _ = 0

-- |z_matrix is the matrix for the Z gate
z_matrix :: Num a => Int -> Int -> a
z_matrix 1 1 = 1
z_matrix 2 2 = -1
z_matrix _ _ = 0

-- |swap_matrix is a matrix that swaps to qubits
swap_matrix :: Num a => Int -> Int -> a
swap_matrix 1 1 = 1
swap_matrix 2 3 = 1
swap_matrix 3 2 = 1
swap_matrix 4 4 = 1
swap_matrix _ _ = 0

-- |w_matrix is the gate_W, the square root of the swap matrix
w_matrix :: (Num a, Floating a) => Int -> Int -> a
w_matrix 1 1 = 1
w_matrix 2 2 = 1
w_matrix 2 3 = 1 / sqrt 2
w_matrix 3 2 = 1 / sqrt 2
w_matrix 3 3 = -1 / sqrt 2
w_matrix 4 4 = 1
w_matrix _ _ = 0
 
