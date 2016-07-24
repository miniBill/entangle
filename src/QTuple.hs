{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module QTuple where

import Quipper

-- |The 'Tuple' class creates a tuple out of a list.
class QTuple a where
    tupleSize :: a -> Int
    tupleFromList :: [Qubit] -> a

instance QTuple Qubit where
    tupleSize _ = 1
    tupleFromList (q1:_) = q1
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 1 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit) where
    tupleSize _ = 2
    tupleFromList (q1:q2:_) = (q1, q2)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 2 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit) where
    tupleSize _ = 3
    tupleFromList (q1:q2:q3:_) = (q1, q2, q3)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 3 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit, Qubit) where
    tupleSize _ = 4
    tupleFromList (q1:q2:q3:q4:_) = (q1, q2, q3, q4)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 4 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit) where
    tupleSize _ = 5
    tupleFromList (q1:q2:q3:q4:q5:_) = (q1, q2, q3, q4,q5)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 5 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    tupleSize _ = 6
    tupleFromList (q1:q2:q3:q4:q5:q6:_) = (q1, q2, q3, q4, q5, q6)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 6 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    tupleSize _ = 7
    tupleFromList (q1:q2:q3:q4:q5:q6:q7:_) = (q1, q2, q3, q4, q5, q6, q7)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 7 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    tupleSize _ = 8
    tupleFromList (q1:q2:q3:q4:q5:q6:q7:q8:_) = (q1, q2, q3, q4, q5, q6, q7, q8)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 8 or more, got" ++ show (length l)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    tupleSize _ = 9
    tupleFromList (q1:q2:q3:q4:q5:q6:q7:q8:q9:_) = (q1, q2, q3, q4, q5, q6, q7, q8, q9)
    tupleFromList l = error $ "Not enough elements passed to tupleFromList, expected 9 or more, got" ++ show (length l)
