module Examples where

import           Qpmc
import           Quipper

----------------------------------------------------------------------

qftInternal :: [Qubit] -> Circ [Qubit]
qftInternal [] = return []
qftInternal [x] = do
  hadamard x
  return [x]
qftInternal (x:xs) = do
  xs' <- qftInternal xs
  xs'' <- rotations x xs' (length xs')
  x' <- hadamard x
  return (x':xs'')
  where
    -- Auxiliary function used by 'qft'.
    rotations :: Qubit -> [Qubit] -> Int -> Circ [Qubit]
    rotations _ [] _ = return []
    rotations c (q:qs) n = do
      qs' <- rotations c qs n
      q' <- rGate ((n + 1) - length qs) q `controlled` c
      return (q':qs')

----------------------------------------------------------------------

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
doubleMeas (q1, q2, _) = measure (q1, q2)

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

groverNaive2 :: (Qubit, Qubit, Qubit) -> Circ (Bit, Bit)
groverNaive2 (q1,q2,q3) = do
    qa <- hadamard q1
    qb <- hadamard q2
    qc <- hadamard q3
    --gate_X_at q2
    qd <- qnot qc `controlled` [qa, qb]
    --gate_X_at q2
    qe <- hadamard qa
    qf <- hadamard qb
    qg <- gate_X qe
    qh <- gate_X qf
    qj <- hadamard qh
    qk <- qnot qj `controlled` qg
    ql <- hadamard qk
    qm <- gate_X qg
    qn <- gate_X ql
    qo <- hadamard qm
    qp <- hadamard qn
    _ <- hadamard qd
    measure (qo,qp)


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
  _ <- dynamic_lift m2
  if bool1
    then
      return (qd,qb)
    else
      recCirc (qd,qb)

recCirc' :: (Qubit, Qubit) -> Circ RecAction
recCirc' (qa, qb) = do
  qc <- hadamard qa
  qd <- qnot qc `controlled` qb
  m1 <- measure qd
  m2 <- measure qb
  bool1 <- dynamic_lift m1
  bool2 <- dynamic_lift m2
  exitOn $ bool1 && bool2
------------------------------

groverSix :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Bit, Bit, Bit, Bit, Bit, Bit)
groverSix (q1,q2,q3, q4, q5, q6, q7) = do
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    hadamard_at q4
    hadamard_at q5
    hadamard_at q6
    hadamard_at q7
    --startOracle
    qnot_at q7 `controlled` [q1, q2, q3, q4, q5, q6]
    --endOracle

    --startRotation
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    hadamard_at q4
    hadamard_at q5
    hadamard_at q6
    gate_X_at q1
    gate_X_at q2
    gate_X_at q3
    gate_X_at q4
    gate_X_at q5
    gate_X_at q6
    hadamard_at q6
    qnot_at q6 `controlled` [q1, q2, q3, q4, q5]
    hadamard_at q6
    gate_X_at q1
    gate_X_at q2
    gate_X_at q3
    gate_X_at q4
    gate_X_at q5
    gate_X_at q6
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    hadamard_at q4
    hadamard_at q5
    hadamard_at q6
    --endRotation

    hadamard_at q7
    measure (q1,q2,q3,q4,q5,q6)

---------------------------------

groverRec :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ RecAction
groverRec (q1,q2,q3, q4, q5, q6, q7) = do
    qa <- hadamard q1
    qb <- hadamard q2
    qc <- hadamard q3
    qd <- hadamard q4
    qe <- hadamard q5
    qf <- hadamard q6
    qg <- hadamard q7
    --startOracle
    qnot_at qg `controlled` [qa, qb, qc, qd, qe, qf]
    --endOracle

    --startRotation
    hadamard_at qa
    hadamard_at qb
    hadamard_at qc
    hadamard_at qd
    hadamard_at qe
    hadamard_at qf
    gate_X_at qa
    gate_X_at qb
    gate_X_at qc
    gate_X_at qd
    gate_X_at qe
    gate_X_at qf
    hadamard_at qf
    qnot_at qf `controlled` [qa, qb, qc, qd, qe]
    hadamard_at qf
    gate_X_at qa
    gate_X_at qb
    gate_X_at qc
    gate_X_at qd
    gate_X_at qe
    gate_X_at qf
    hadamard_at qa
    hadamard_at qb
    hadamard_at qc
    hadamard_at qd
    hadamard_at qe
    hadamard_at qf
    --endRotation

    hadamard_at qg
    m1 <- measure qa
    m2 <- measure qb
    m3 <- measure qc
    m4 <- measure qd
    m5 <- measure qe
    m6 <- measure qf
    m7 <- measure qg

    bool1 <- dynamic_lift m1
    bool2 <- dynamic_lift m2
    bool3 <- dynamic_lift m3
    bool4 <- dynamic_lift m4
    bool5 <- dynamic_lift m5
    bool6 <- dynamic_lift m6
    bool7 <- dynamic_lift m7

    --if bool1 && (not bool2) && (not bool3) && (not bool4) && (not bool5) && (not bool6)
    --   then return (qa,qb,qc,qd,qe,qf,qg)
    --   else groverRec (qa,qb,qc,qd,qe,qf,qg)
    exitOn $ bool1 && not bool2 && not bool3 && not bool4 && not bool5 && not bool6 && not bool7

groverRecFive :: (Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ RecAction
groverRecFive (q1,q2,q3, q4, q5) = do
    qa <- hadamard q1
    qb <- hadamard q2
    qc <- hadamard q3
    qd <- hadamard q4
    qe <- hadamard q5

    --startOracle
    qnot_at qe `controlled` [qa, qb, qc, qd]
    --endOracle

    --startRotation
    hadamard_at qa
    hadamard_at qb
    hadamard_at qc
    hadamard_at qd

    gate_X_at qa
    gate_X_at qb
    gate_X_at qc
    gate_X_at qd

    hadamard_at qd
    qnot_at qe `controlled` [qa, qb, qc, qd]
    hadamard_at qd

    gate_X_at qa
    gate_X_at qb
    gate_X_at qc
    gate_X_at qd

    hadamard_at qa
    hadamard_at qb
    hadamard_at qc
    hadamard_at qd

    hadamard_at qe

    m1 <- measure qa
    m2 <- measure qb
    m3 <- measure qc
    m4 <- measure qd
    m5 <- measure qe

    bool1 <- dynamic_lift m1
    bool2 <- dynamic_lift m2
    bool3 <- dynamic_lift m3
    bool4 <- dynamic_lift m4
    bool5 <- dynamic_lift m5

    --if bool1 && (not bool2) && (not bool3) && (not bool4) && (not bool5) && (not bool6)
    --   then return (qa,qb,qc,qd,qe,qf,qg)
    --   else groverRec (qa,qb,qc,qd,qe,qf,qg)
    exitOn $ bool1 && not bool2 && not bool3 && not bool4 && not bool5


----------------------------

branchCirc :: (Qubit, Qubit) -> Circ RecAction
branchCirc (qa, qb) = do
    hadamard_at qa
    m <- measure qb
    bool <- dynamic_lift m
    if bool
       then hadamard_at qa
       else qnot_at qa
    exitOn bool

qkdCirc :: (Qubit, Qubit) -> Circ RecAction
qkdCirc (qa, qb) = do
    hadamard_at qa
    m <- measure qb
    bool <- dynamic_lift m
    if bool
       then gate_X_at qa
       else gate_Z_at qa
    m1 <- measure qa
    exitOn bool


{-branchCircBoth :: (Qubit, Qubit) -> Circ (Qubit, Qubit, Bool)
branchCircBoth (qa, qb) = do
    hadamard_at qa
    m <- measure qb
    bool <- dynamic_lift m
    if bool
       then hadamard_at qa
       else qnot_at qa
    return (qa, qb, bool)

branchCircQuipper (qa, qb) = do
  (qa, qb, _) <- branchCircBoth
  return (qa, qb)

branchCircQpmc (qa, qb) = do
  (_, _, bool) <- branchCircBoth
  exitOn bool-}

interfCirc :: (Qubit, Qubit) -> Circ RecAction
interfCirc (qa, qb) = do
  hadamard_at qa
  rGate_at 2 qb `controlled` qa
  qftInternal [qa,qb]
  ma <- measure qa
  mb <- measure qb
  boola <- dynamic_lift ma
  boolb <- dynamic_lift mb
  exitOn $ boola == boolb
