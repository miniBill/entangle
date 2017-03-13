{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import           Data.Aeson.Types
import           Data.Matrix                 (Matrix)
import           Network.Wai.Middleware.Cors
import           Quipper
import           Web.Scotty

import           Complex
import           Examples
import           Expr
import           QMatrix
import           Qpmc
import           QTuple
import           SymbolicMatrix
import           Transitions

-- |fullOut takes a function returning a value in the 'Circ' monad,
-- and outputs the result of transforming it to QPMC code
--fullOut :: QTuple a => (a -> Circ b) -> IO ()
fullOut :: (QTuple a, Show b, QCMatrix m Expr, ToQpmc (m (Complex Expr))) => m x -> (b -> [Transition m Expr]) -> (a -> Circ b) -> IO ()
fullOut _ final c = do
    putStr "---\n"
    let tree = circToTree c
    print tree
    putStr "---\n"
    let transitions = circMatrices final c
    putStrLn $ toQpmc transitions
    putStr "---\n"

nonrecursive :: a -> [Transition m Expr]
nonrecursive = const []

recursive :: RecAction -> [Transition m v]
recursive Exit = []
recursive Loop = [Transition Nothing $ StateName 0 []]

symbolic :: SymbolicMatrix a
symbolic = error "proxy"

numeric :: Matrix a
numeric = error "proxy"

-- main :: IO ()
-- main = fullOut
--   symbolic
--   --numeric

--   --nonrecursive grover_naive
--   --nonrecursive test_matrix_3
--   --nonrecursive test_matrix_3
--   --nonrecursive strange
--   --nonrecursive mycirc
--   --nonrecursive test_if
--   --recursive recCirc'
--   --recursive branchCirc
--   --recursive interfCirc
--   recursive groverRec

data Data = Data { code :: String }

instance FromJSON Data where
  parseJSON (Object v) =
    Data <$>       v .: "code"
  parseJSON invalid =typeMismatch "Data" invalid

instance ToJSON Data where
  toJSON (Data code) = object ["code" .= code]

root :: ActionM ()
root = (do
  d <- jsonData :: ActionM Data
  json d) `rescue` text

main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  get "/" $ text "Welcome to entangle!"
  post "/" root
