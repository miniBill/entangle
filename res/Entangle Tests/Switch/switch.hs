switchProg :: (Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
switchProg (qc1,qc2,qa, qb,qx) = do

    hadamard_at qa
    hadamard_at qb
    hadamard_at qx

    c1 <- measure qc1
    c2 <- measure qc2
    bool1 <- dynamic_lift c1
    bool2 <- dynamic_lift c2
    if (not bool1) && (not bool2)
       then 
	   gate_X_at qa
           gate_X_at qb
           qnot_at qx `controlled` [qa,qb]
           gate_X_at qa
           gate_X_at qb

        else if bool1 && (not bool2)
             then 
               gate_X_at qa
               qnot_at qx `controlled` [qa,qb]
               gate_X_at qa

        else if (not bool1) && bool2
             then 
               gate_X_at qb
               qnot_at qx `controlled` [qa,qb]
               gate_X_at qb

        else
               qnot_at qx `controlled` [qa,qb]

    -- Resto del circuito (applicato solo su qa,qb,qx) --

    hadamard_at qa
    hadamard_at qb
    gate_X_at qa
    gate_X_at qb
    hadamard_at qb
    qnot_at qb `controlled` qa
    hadamard_at qb
    gate_X_at qa
    gate_X_at qb
    hadamard_at qa
    hadamard_at qb
    hadamard_at qx
    return (qa,qb,qx)




switchCirc :: (Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
switchCirc (qc1,qc2,qa, qb,qx) = do
    
    hadamard_at qa
    hadamard_at qb
    hadamard_at qx
    
    --------------
    
    gate_X_at qc1
    gate_X_at qc2

    gate_X_at qa
    gate_X_at qb
    qnot_at qx `controlled` [qa,qb, qc1, qc2]
    gate_X_at qa
    gate_X_at qb
    
    gate_X_at qc1
    gate_X_at qc2
    
    --------------
    
    gate_X_at qc1

    gate_X_at qa
    qnot_at qx `controlled` [qa,qb, qc1, qc2]
    gate_X_at qa
    
    gate_X_at qc1

    --------------
    
    gate_X_at qc2

    gate_X_at qb
    qnot_at qx `controlled` [qa,qb, qc1, qc2]
    gate_X_at qb

    gate_X_at qc2

    --------------

    qnot_at qx `controlled` [qa,qb, qc1, qc2]

    -- Resto del circuito (applicato solo su qa,qb,qx) --

    hadamard_at qa
    hadamard_at qb
    gate_X_at qa
    gate_X_at qb
    hadamard_at qb
    qnot_at qb `controlled` qa
    hadamard_at qb
    gate_X_at qa
    gate_X_at qb
    hadamard_at qa
    hadamard_at qb
    hadamard_at qx
    return (qa,qb,qx)



