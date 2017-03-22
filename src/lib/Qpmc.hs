{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Qpmc where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Matrix    (Matrix, toLists)
import           Data.Maybe

import           Complex
import           Expr
import           SymbolicMatrix hiding (eval)
import           Transitions


data RecAction = Loop | Exit deriving Show

exitOn :: Monad m => Bool -> m RecAction
exitOn True  = return Exit
exitOn False = return Loop

nonrecursive :: a -> [Transition m Expr]
nonrecursive = const []

recursive :: RecAction -> [Transition m v]
recursive Exit = []
recursive Loop = [Transition Nothing $ StateName 0 []]

symbolic :: SymbolicMatrix Expr
symbolic = error "proxy"

numeric :: Matrix (Complex Expr)
numeric = error "proxy"

class ToQpmc a where
    toQpmc :: a -> String

instance (Floating a, Show a) => ToQpmc (SymbolicMatrix a) where
    toQpmc = show

instance Show a => ToQpmc (Matrix a) where
    toQpmc mat =
        let
            sl l = intercalate "," $ map show l
            inner = intercalate ";" $ map sl $ toLists mat
        in
            "[" ++ inner ++ "]"

instance ToQpmc (m (Complex a)) => ToQpmc (String, [Transitions m a]) where
    toQpmc (name, ts) = "qmc\n"
            ++ concatMap transitionToMatrix (concatMap trDestinations ts)
            ++ "module " ++ name ++ "\n"
            ++ "  s: [0.." ++ show (foldr (max . snId) 0 named) ++ "] init 0;\n"
            ++ concatMap (\i -> "  b" ++ show i ++ ": bool init false;\n") [0..bs-1]
            ++ concatMap toQpmc (sortBy tsort ts)
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
transitionToMatrix :: ToQpmc (m (Complex a)) => Transition m a -> String
transitionToMatrix t = fromMaybe "" $ do
    mat <- trMatrix t
    let inner = toQpmc mat
    let res = "const matrix A" ++ show (trToState t) ++ " = " ++ inner ++ ";\n"
    return res

instance ToQpmc (Transitions m a) where
    toQpmc (Transitions f ds) = "  [] " ++ stateNameToQpmcGuard f ++ " -> " ++ transitions ++ ";\n" where
        transitions = intercalate " + " $ map (transitionToQpmc (length $ snBs f)) ds
        transitionToQpmc prefix (Transition Nothing n) = stateNameToQpmcDestination prefix n
        transitionToQpmc prefix (Transition (Just _) n) = "<<A" ++ show n ++ ">> : " ++ stateNameToQpmcDestination prefix n
