{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Qpmc where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe

import           MatrixExtra
import           SqMath
import           Transitions

-- |to_qmc takes a list of transitions and returns their representation in QPMC code
toQpmc :: GCMatrix m Expr => [Transitions m Expr] -> String
toQpmc ts = "qmc\n"
          ++ concatMap transitionToMatrix (concatMap trDestinations ts)
          ++ "module test\n"
          ++ "  s: [0.." ++ show (foldr (max . snId) 0 named) ++ "] init 0;\n"
          ++ concatMap (\i -> "  b" ++ show i ++ ": bool init false;\n") [0..bs-1]
          ++ concatMap transitionsToQpmc (sortBy tsort ts)
          ++ concatMap finalToQpmc finals
          ++ "endmodule" where
    bs = foldr (max . length . snBs . trToState) 0 $ concatMap trDestinations ts
    named :: [StateName]
    named = concatMap (map trToState . trDestinations) ts
    finals :: [StateName]
    finals = filter (\(StateName i _) -> i > 0) $ named \\ map trFromState ts

tsort :: Transitions m a -> Transitions m a -> Ordering
tsort = compare `on` trFromState

stateNameToQpmcGuard :: StateName -> String
stateNameToQpmcGuard (StateName i bs) = "(s = " ++ show i ++ ")" ++ booleans where
    booleans = concatMap (\(b,j) -> " & " ++ (if b then "" else "!") ++ "b" ++ show j) (zip bs [0..])

stateNameToQpmcDestination :: Int -> StateName -> String
stateNameToQpmcDestination prefix (StateName i bs) = "(s' = " ++ show i ++ ")" ++ booleans where
    booleans = concatMap (\(b,j) -> " & " ++ "(b" ++ show j ++ "' = " ++ showLower b ++ ")") (drop prefix $ zip bs [0..])

showLower :: Show a => a -> String
showLower = map toLower . show

-- |finalToQpmc returns the QPMC code for a final state
finalToQpmc :: StateName -> String
finalToQpmc s = "  [] " ++ stateNameToQpmcGuard s ++ " -> true;\n"

-- |transitionToMatrix returns the QPMC code for a matrix
transitionToMatrix :: (GCMatrix m a, Show a) => Transition m a -> String
transitionToMatrix t = fromMaybe "" $ do
    mat <- trMatrix t
    let inner = matrixToQpmc mat
    let res = "const matrix A" ++ show (trToState t) ++ " = " ++ inner ++ ";\n"
    return res

-- |transitionsToQpmc returns the QPMC code for a transition
transitionsToQpmc :: Transitions m a -> String
transitionsToQpmc (Transitions f ds) = "  [] " ++ stateNameToQpmcGuard f ++ " -> " ++ transitions ++ ";\n" where
    transitions = intercalate " + " $ map (transitionsToQpmc' (length $ snBs f)) ds

-- |transitionsToQpmc' is an helper function used by 'transitionsToQpmc'
transitionsToQpmc' :: Int -> Transition m a -> String
transitionsToQpmc' prefix (Transition Nothing n) = stateNameToQpmcDestination prefix n
transitionsToQpmc' prefix (Transition (Just _) n) = "<<A" ++ show n ++ ">> : " ++ stateNameToQpmcDestination prefix n
