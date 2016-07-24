module Qpmc where

import Data.List
import Data.Matrix

import SqMath
import Transitions

-- |to_qmc takes a list of transitions and returns their representation in QPMC code
toQpmc :: [Transitions Expr] -> String
toQpmc ts = "qmc\n"
          ++ concatMap transitionToMatrix (concatMap trDestinations ts)
          ++ "module test\n"
          ++ "  s: [0.." ++ show (foldr (max . snId) 0 named) ++ "] init 0;\n"
          ++ concatMap (\i -> "  b" ++ show i ++ "bool init false;\n") [0..bs-1]
          ++ concatMap transitionsToQpmc ts
          ++ concatMap finalToQpmc finals
          ++ "endmodule" where
    bs = foldr (max . length . snBs . trToState) 0 $ concatMap trDestinations ts
    named :: [StateName]
    named = concatMap (map trToState . trDestinations) ts
    finals :: [StateName]
    finals = named \\ map trFromState ts

stateNameToQpmcGuard :: StateName -> String
stateNameToQpmcGuard (StateName i bs) = "(s = " ++ show i ++ ") " ++ booleans where
    booleans = concatMap (\(b,j) -> "& " ++ (if b then "" else "!") ++ "b" ++ show j) (zip bs [0..])

stateNameToQpmcDestination :: StateName -> String
stateNameToQpmcDestination (StateName i bs) = "(s = " ++ show i ++ ") " ++ booleans where
    booleans = concatMap (\(b,j) -> "& " ++ "(b" ++ show j ++ " = " ++ (if b then "true" else "false")) (zip bs [0..])

-- |finalToQpmc returns the QPMC code for a final state
finalToQpmc :: StateName -> String
finalToQpmc s = "  [] " ++ stateNameToQpmcGuard s ++ " -> true;\n"

-- |transitionToMatrix returns the QPMC code for a matrix
transitionToMatrix :: Show a => Transition a -> String
transitionToMatrix t = "const matrix A" ++ show (trToState t) ++ " = [" ++ inner ++ "];\n" where
    inner = intercalate ";" $ map sl $ toLists (trMatrix t)
    sl l = intercalate "," $ map show l

-- |transitionsToQpmc returns the QPMC code for a transition
transitionsToQpmc :: Transitions a -> String
transitionsToQpmc (Transitions f ds) = "  [] " ++ stateNameToQpmcGuard f ++ " -> " ++ transitions ++ ";\n" where
    transitions = intercalate " + " $ map (transitionsToQpmc' . trToState) ds

-- |transitionsToQpmc' is an helper function used by 'transitionsToQpmc'
transitionsToQpmc' :: StateName -> String
transitionsToQpmc' n = "<<A" ++ show n ++ ">> : " ++ stateNameToQpmcDestination n
