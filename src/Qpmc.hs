module Qpmc where

import Data.List
import Data.Matrix

import EntangleMonad
import SqMath

-- |to_qmc takes a list of transitions and the maximum number of booleans used
-- and returns their representation in QPMC code
toQpmc :: [Transitions Expr] -> Int -> String
toQpmc ts bs = "qmc\n"
--           const matrix asdf = [1,2;3,4];
--           "mf2so([1,0,0,0; 0,0,0,0; 0,0,0,0; 0,0,0,0])
          ++ concatMap matrixToQmc (concatMap trDestinations ts)
          ++ "module test\n"
          ++ "  s: [0.." ++ show (foldr (max . snId) 0 named) ++ "] init 0;\n"
          ++ concatMap (\i -> "  b" ++ show i ++ "bool init false;\n") [0..bs-1]
          ++ concatMap transitionToQmc ts
          ++ concatMap finalToQpmc finals
          ++ "endmodule" where
    named :: [StateName]
    named = concatMap (map fst . trDestinations) ts
    finals :: [StateName]
    finals = named \\ map trFromState ts

stateNameToQpmcGuard :: StateName -> String
stateNameToQpmcGuard (StateName i bs) = "(s = " ++ show i ++ ") " ++ booleans where
    booleans = concatMap (\(b,j) -> "& " ++ (if b then "" else "!") ++ "b" ++ show j) (zip bs [0..])

stateNameToQpmcDestination :: StateName -> String
stateNameToQpmcDestination (StateName i bs) = "(s = " ++ show i ++ ") " ++ booleans where
    booleans = concatMap (\(b,j) -> "& " ++ (if b then "" else "!") ++ "b" ++ show j) (zip bs [0..])

-- |finalToQpmc returns the QPMC code for a final state
finalToQpmc :: StateName -> String
finalToQpmc s = "  [] " ++ stateNameToQpmcGuard s ++ " -> true;\n"

-- |matrixToQmc returns the QPMC code for a matrix
matrixToQmc :: Show a => (StateName, Matrix a) -> String
matrixToQmc (t, ms) = "const matrix A" ++ show t ++ " = [" ++ inner ++ "];\n" where
    inner = intercalate ";" $ map sl $ toLists ms
    sl l = intercalate "," $ map show l

-- |transitionToQmc returns the QPMC code for a transition
transitionToQmc :: Transitions a -> String
transitionToQmc (Transitions f ds) = "  [] " ++ stateNameToQpmcGuard f ++ " -> " ++ transitions ++ ";\n" where
    transitions = intercalate " + " (map (transitionToQmc' . fst) ds)

-- |transitionToQmc' is an helper function used by 'transitionToQmc'
transitionToQmc' :: StateName -> String
transitionToQmc' n = "<<A" ++ show n ++ ">> : " ++ stateNameToQpmcDestination n
