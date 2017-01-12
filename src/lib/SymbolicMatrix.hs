{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SymbolicMatrix (
    SymbolicMatrix,
    eval, evalC
    ) where

import           Data.List

import           Complex
import           QMatrix
import           Qpmc

data StandardMatrix a where
    Identity :: Integer -> StandardMatrix a
    Hadamard :: StandardMatrix a
    PauliX :: StandardMatrix a
    PauliZ :: StandardMatrix a
    -- ControlNot :: StandardMatrix a
    Swap :: StandardMatrix a
    PhaseShift :: a -> StandardMatrix (Complex a)
    Measure :: MeasureKind -> StandardMatrix a

data SymbolicMatrix a
    = StandardMatrix (StandardMatrix a)
    | Zero Integer Integer
    | Matrix Integer Integer (Integer -> Integer -> a)
    | Multiply (SymbolicMatrix a) (SymbolicMatrix a)
    | Kronecker (SymbolicMatrix a) (SymbolicMatrix a)

instance Show (StandardMatrix a) where
    show (Identity i) = "identity(" ++ show i ++ ")"
    show Hadamard     = "Hadamard"
    show PauliX       = "PauliX"
    show PauliZ       = "PauliZ"
    --show ControlNot     = "CNOT"
    show Swap         = "Swap"
    show (Measure UL) = "M0"
    show (Measure BR) = "M1"

instance Show a => Show (StandardMatrix (Complex a)) where
    show (Identity i)   = "identity(" ++ show i ++ ")"
    show Hadamard       = "Hadamard"
    show PauliX         = "PauliX"
    show PauliZ         = "PauliZ"
    --show ControlNot     = "CNOT"
    show Swap           = "Swap"
    show (Measure UL)   = "M0"
    show (Measure BR)   = "M1"
    show (PhaseShift d) = "PhaseShift(" ++ show d ++ ")"

instance (Floating a, Show a) => Show (SymbolicMatrix a) where
    show (StandardMatrix m) = show m
    show (Zero r c) = show $ Matrix r c $ \_ _ -> 0
    show (Matrix r c f) =
        let
            showRow y = intercalate ", " $ map (show . f y) [1..c]
            rows = intercalate "; " $ map showRow [1..r]
        in
            "[" ++ rows ++ "]"
    show (Kronecker a b) = "kron (" ++ show a ++ ", " ++ show b ++ ")"
    show m@(Multiply _ _)  =
        let
            (ExplodedMatrix r c f) = eval m
        in
            show $ Matrix r c f

instance Num (SymbolicMatrix a) where
    (*) (StandardMatrix (Identity _)) b = b
    (*) a (StandardMatrix (Identity _)) = a
    (*) a b                             = Multiply a b

    (+) = error "+ undefined for SymbolicMatrix"
    (-) = error "- undefined for SymbolicMatrix"
    abs = error "abs undefined for SymbolicMatrix"
    signum = error "signum undefined for SymbolicMatrix"
    fromInteger = error "fromInteger undefined for SymbolicMatrix"

instance Floating a => QMatrix SymbolicMatrix a where
    kronecker a (StandardMatrix (Identity 1)) = a
    kronecker (StandardMatrix (Identity 1)) b = b
    kronecker a b                             = Kronecker a b

    identity = StandardMatrix . Identity

    zero = Zero

    matrix = Matrix
    (<->) = liftEval (+) min $ \(ExplodedMatrix r1 _ m1) (ExplodedMatrix _ _ m2) r c -> if r <= r1 then m1 r c else m2 (r - r1) c
    (<|>) = liftEval min (+) $ \(ExplodedMatrix _ c1 m1) (ExplodedMatrix _ _ m2) r c -> if c <= c1 then m1 r c else m2 r (c - c1)

    hadamard = StandardMatrix Hadamard
    pauliX = StandardMatrix PauliX
    pauliZ = StandardMatrix PauliZ
    swap = StandardMatrix Swap
    measure = StandardMatrix . Measure

data ExplodedMatrix a = ExplodedMatrix Integer Integer (Integer -> Integer -> a)

instance Floating a => QMatrix ExplodedMatrix a where
    matrix = ExplodedMatrix
    kronecker (ExplodedMatrix ra ca fa) (ExplodedMatrix rb cb fb) =
        let
            gen r c = ae * be where
                ae = fa ar ac
                ar = 1 + (r - 1) `div` rb
                ac = 1 + (c - 1) `div` cb
                be = fb br bc
                br = 1 + (r - 1) `mod` rb
                bc = 1 + (c - 1) `mod` cb
        in
            ExplodedMatrix (ra * rb) (ca * cb) gen

    (<->) = liftEvalE (+) min $ \(ExplodedMatrix r1 _ m1) (ExplodedMatrix _ _ m2) r c -> if r <= r1 then m1 r c else m2 (r - r1) c
    (<|>) = liftEvalE min (+) $ \(ExplodedMatrix _ c1 m1) (ExplodedMatrix _ _ m2) r c -> if c <= c1 then m1 r c else m2 r (c - c1)

instance Num a => Num (ExplodedMatrix a) where
    (ExplodedMatrix ra ca fa) * (ExplodedMatrix rb cb fb) =
        let
            gen r c = sum $ zipWith (*) (map (fa r) [1..ca]) (map (`fb` c) [1..rb])
        in
            ExplodedMatrix ra cb gen

    (+) = error "+ undefined for ExplodedMatrix"
    (-) = error "- undefined for ExplodedMatrix"
    abs = error "abs undefined for ExplodedMatrix"
    signum = error "signum undefined for ExplodedMatrix"
    fromInteger = error "fromInteger undefined for ExplodedMatrix"

liftEval :: Floating a =>
    (Integer -> Integer -> Integer) ->
    (Integer -> Integer -> Integer) ->
    (ExplodedMatrix a -> ExplodedMatrix a -> Integer -> Integer -> a) ->
    (SymbolicMatrix a -> SymbolicMatrix a -> SymbolicMatrix a)
liftEval rf cf vf a b =
    let
        xa@(ExplodedMatrix ra ca _) = eval a
        xb@(ExplodedMatrix rb cb _) = eval b
        f = vf xa xb
    in
        Matrix (rf ra rb) (cf ca cb) f

liftEvalE :: QMatrix m a =>
    (Integer -> Integer -> Integer) ->
    (Integer -> Integer -> Integer) ->
    (ExplodedMatrix a -> ExplodedMatrix a -> Integer -> Integer -> a) ->
    (ExplodedMatrix a -> ExplodedMatrix a -> m a)
liftEvalE rf cf vf xa@(ExplodedMatrix ra ca _) xb@(ExplodedMatrix rb cb _) =
    let
        f = vf xa xb
    in
        matrix (rf ra rb) (cf ca cb) f

instance (Floating a, Fractional a) => QCMatrix SymbolicMatrix a where
    phaseShift t = StandardMatrix $ PhaseShift t

instance (Floating a, Show a) => ToQpmc (SymbolicMatrix a) where
    toQpmc = show

eval :: (Floating a, QMatrix m a) => SymbolicMatrix a -> m a
eval (Zero r c) = zero r c
eval (Matrix r c f) = matrix r c f
eval (Kronecker a b) = kronecker (eval a) (eval b)
eval (Multiply a b) = eval a * eval b
eval (StandardMatrix m) = eval' m where
    eval' (Identity i) = identity i
    eval' Hadamard     = hadamard
    eval' PauliX       = pauliX
    eval' PauliZ       = pauliZ
    eval' Swap         = swap
    eval' (Measure k)  = measure k

evalC :: QCMatrix m a => SymbolicMatrix (Complex a) -> m (Complex a)
evalC (Zero r c) = zero r c
evalC (Matrix r c f) = matrix r c f
evalC (Kronecker a b) = kronecker (evalC a) (evalC b)
evalC (Multiply a b) = evalC a * evalC b
evalC (StandardMatrix (PhaseShift d)) = phaseShift d
evalC (StandardMatrix m) = evalC' m where
    evalC' (Identity i) = identity i
    evalC' Hadamard     = hadamard
    evalC' PauliX       = pauliX
    evalC' PauliZ       = pauliZ
    evalC' Swap         = swap
    evalC' (Measure k)  = measure k
