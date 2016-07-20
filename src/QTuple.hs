{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module QTuple where

import Quipper

-- |The 'Tuple' class creates a tuple out of a list.
class QTuple a where
    size :: a -> Int
    tupleFromList :: [Qubit] -> a

instance QTuple Qubit where
    size _ = 1
    tupleFromList = head

instance QTuple (Qubit, Qubit) where
    size _ = 2
    tupleFromList (q1:q2:_) = (q1, q2)

instance QTuple (Qubit, Qubit, Qubit) where
    size _ = 3
    tupleFromList (q1:q2:q3:_) = (q1, q2, q3)

instance QTuple (Qubit, Qubit, Qubit, Qubit) where
    size _ = 4
    tupleFromList (q1:q2:q3:q4:_) = (q1, q2, q3, q4)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit) where
    size _ = 5
    tupleFromList (q1:q2:q3:q4:q5:_) = (q1, q2, q3, q4,q5)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    size _ = 6
    tupleFromList (q1:q2:q3:q4:q5:q6:_) = (q1, q2, q3, q4, q5, q6)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    size _ = 7
    tupleFromList (q1:q2:q3:q4:q5:q6:q7:_) = (q1, q2, q3, q4, q5, q6, q7)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    size _ = 8
    tupleFromList (q1:q2:q3:q4:q5:q6:q7:q8:_) = (q1, q2, q3, q4, q5, q6, q7,q8)

instance QTuple (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) where
    size _ = 9
    tupleFromList (q1:q2:q3:q4:q5:q6:q7:q8:q9:_) = (q1, q2, q3, q4, q5, q6, q7,q8,q9)
