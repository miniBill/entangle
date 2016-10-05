module Examples where

import Quipper

data RecAction = Loop | Exit deriving Show

exitOn :: Bool -> Circ RecAction
exitOn True = return Exit
exitOn False = return Loop

myfourthcirc :: Qubit -> Circ Qubit
myfourthcirc q1 = do
    hadamard q1
    qnot q1
    hadamard q1
    qnot q1
    return q1

mythirdcirc :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
mythirdcirc (q1, q2, q3) = do
    qnot_at q2 `controlled` q1
    qnot_at q2 `controlled` q3
    qnot_at q2 `controlled` q1
    qnot_at q2 `controlled` q3
    return (q1, q2, q3)

myothercirc :: Qubit -> Circ Qubit
myothercirc q1 = do
    hadamard q1
    hadamard q1
    return q1

mycirc :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
mycirc (q1, q2, q3, q4, q5, q6) = do
    qnot_at q1 `controlled` q6
    qnot_at q2 `controlled` q3
    qnot_at q5 `controlled` q6
    qnot_at q4 `controlled` q5
    qnot_at q3 `controlled` q4
    gate_W q1 q3
    return (q1, q2, q3, q4, q5, q6)

deutsch :: (Qubit, Qubit) -> Circ Bit
deutsch (q1, q2) = do
    hadamard q1
    hadamard q2
    qnot_at q2 `controlled` q1
    hadamard q1
    measure q1

deutschJozsaNaive :: (Qubit, Qubit, Qubit) -> Circ (Bit, Bit)
deutschJozsaNaive (q1, q2, q3) = do
    hadamard q1
    hadamard q2
    hadamard q3
    --qnot_at q3 `controlled` [q1,q2]
    --qnot_at q2 `controlled` [q1,q3]
    hadamard q1
    hadamard q2
    measure (q1, q2)

circW :: (Qubit, Qubit) -> Circ (Bit, Bit)
circW (q1, q2) = do
    gate_W q1 q2
    gate_W q2 q1
    measure (q1, q2)

oneq :: Qubit -> Circ Qubit
oneq q1 = do
  hadamard_at q1
  return q1


doubleMeas :: (Qubit, Qubit, Qubit) -> Circ (Bit, Bit)
doubleMeas (q1, q2, q3) = measure (q1, q2)

strange :: (Qubit, Qubit) -> Circ (Bit, Bit)
strange (q1, q2) = do
  c2 <- measure q2
  hadamard q1
  hadamard q1
  c1 <- measure q1
  return (c1, c2)

invCnot :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
invCnot (q1, q2) = do
    qnot_at q1 `controlled` q2
    qnot_at q2 `controlled` q1
    --qnot_at q1 `controlled` q2
    return (q1, q2)

testMultiple :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
testMultiple (q1, q2, q3) = do
    gate_W q1 q2
    gate_W q2 q1
    qnot_at q1 `controlled` q3
    return (q1, q2, q3)

groverNaive :: (Qubit, Qubit, Qubit) -> Circ (Bit, Bit)
groverNaive (q1,q2,q3) = do
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    --gate_X_at q2
    qnot_at q3 `controlled` [q1, q2]
    --gate_X_at q2
    hadamard_at q1
    hadamard_at q2
    gate_X_at q1
    gate_X_at q2
    hadamard_at q2
    qnot_at q2 `controlled` q1
    hadamard_at q2
    gate_X_at q1
    gate_X_at q2
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    measure (q1,q2)


testMatrix_6 :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
testMatrix_6 (q1, q2, q3, q4, q5, q6) = do
    qnot_at q1 `controlled` q6
    qnot_at q1 `controlled` q5
    qnot_at q1 `controlled` q6
    qnot_at q1 `controlled` q4
    qnot_at q1 `controlled` q6
    qnot_at q1 `controlled` q3
    qnot_at q1 `controlled` q6
    qnot_at q1 `controlled` q2
    qnot_at q1 `controlled` q6
    return (q1,q2,q3,q4,q5,q6)


testMatrix_5 :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
testMatrix_5 (q1, q2, q3, q4, q5, q6, q7) = do
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q6
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q5
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q4
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q3
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q2
    qnot_at q1 `controlled` q7
    return (q1,q2,q3,q4,q5,q6,q7)

testMatrix_4 :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
testMatrix_4 (q1, q2, q3, q4, q5, q6, q7, q8) = do
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q7
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q6
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q5
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q4
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q3
    qnot_at q1 `controlled` q8
    qnot_at q1 `controlled` q2
    qnot_at q1 `controlled` q8
    return (q1,q2,q3,q4,q5,q6,q7,q8)

testMatrix_3 :: (Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit)
testMatrix_3 (q1, q2, q3, q4, q5) = do
    qnot_at q1 `controlled` q5
    qnot_at q1 `controlled` q4
    qnot_at q1 `controlled` q5
    qnot_at q1 `controlled` q3
    qnot_at q1 `controlled` q5
    qnot_at q1 `controlled` q2
    qnot_at q1 `controlled` q5
    return (q1,q2,q3,q4,q5)

testMatrix_2 :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
testMatrix_2 (q1, q2, q3, q4) = do
    qnot_at q1 `controlled` q4
    qnot_at q1 `controlled` q3
    qnot_at q1 `controlled` q4
    qnot_at q1 `controlled` q2
    qnot_at q1 `controlled` q4
    return (q1,q2,q3,q4)

testMatrix_1 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
testMatrix_1 (q1, q2, q3) = do
    qnot_at q1 `controlled` q3
    qnot_at q1 `controlled` q2
    qnot_at q1 `controlled` q3
    return (q1,q2,q3)

test_if :: (Qubit, Qubit, Qubit) -> Circ Qubit
test_if (q1, q2, q3) = do
    m1 <- measure q1
    bool1 <- dynamic_lift m1
    return $ if bool1 then q2 else q3

recCirc :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
recCirc (qa,qb) = do
  qc <- hadamard qa
  qd <- qnot qc `controlled` qb
  m1 <- measure qc
  m2 <- measure qb
  bool1 <- dynamic_lift m1
  bool2 <- dynamic_lift m2
  if bool1
    then
      return (qd,qb)
    else
      recCirc (qd,qb)

recCirc' :: (Qubit, Qubit) -> Circ RecAction
recCirc' (qa, qb) = do
  qc <- hadamard qa
  qd <- qnot qc `controlled` qb
  m1 <- measure qc
  m2 <- measure qb
  bool1 <- dynamic_lift m1
  bool2 <- dynamic_lift m2
  exitOn $ bool1 && bool2

branchCirc :: (Qubit, Qubit) -> Circ RecAction
branchCirc (qa, qb) = do
    hadamard_at qa
    m <- measure qb
    bool <- dynamic_lift m
    if bool
       then hadamard_at qa
       else qnot_at qa
    exitOn bool
