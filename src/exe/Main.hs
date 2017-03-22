{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import           Data.Aeson.Types
import           Data.List
import           Data.Monoid                  ((<>))
import           Language.Haskell.Interpreter hiding (get)
import           Network.Wai                  hiding (Request, Response)
import           Network.Wai.Middleware.Cors
import           Web.Scotty                   hiding (request)

data Request = Request {
  rName      :: String,
  rType      :: String,
  rCode      :: String,
  rRecursive :: Bool,
  rKind      :: String
}

instance FromJSON Request where
  parseJSON =  withObject "Request" $ \v -> Request
    <$> v .: "name"
    <*> v .: "type"
    <*> v .: "code"
    <*> v .: "recursive"
    <*> v .: "kind"

data Response = Response {
  rQpmc :: String,
  rTree :: String
}

instance ToJSON Response where
  toJSON (Response qpmc  tree) =
    object ["qpmc" .= qpmc, "tree" .= tree]

  toEncoding (Response qpmc tree) =
    pairs ("qpmc" .= qpmc <> "tree" .= tree)

errorString :: InterpreterError -> String
errorString (WontCompile es) =
  let
    unbox (GhcError e) = e
  in
    intercalate "\n" ("ERROR: Won't compile:" : map unbox es)
errorString e = show e

useHint :: String -> ActionM String
useHint input =
  let
    result = runInterpreter $ do
      setImports
        [ "Prelude"
        , "Quipper"
        , "Transitions"
        , "Qpmc"
        , "Expr"
        , "SymbolicMatrix"
        , "Data.Matrix"
        ]
      interpret input ""
  in
    liftAndCatchIO $ either errorString id <$> result

root :: ActionM ()
root = do
  request <- jsonData :: ActionM Request
  let name = rName request
  let code = rCode request
  let type_ = rType request
  let f = concat ["(let ", name, " = ", code, " in ", name, " :: ", type_, ")"]
  let treeCode = "show $ circToTree " ++ f
  tree <- useHint treeCode
  let final = if rRecursive request then "recursive" else "nonrecursive"
  let kind = rKind request
  let qpmcCode = concat ["toQpmc (", show name, ", circMatrices ", final, " ", kind, " ",  f, ")"]
  qpmc <- useHint qpmcCode
  json $ Response qpmc tree

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = ["content-type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

main :: IO ()
main = scotty 3113 handler

dev :: (Application -> IO a) -> IO a
dev h = scottyApp handler >>= h

handler :: ScottyM ()
handler = do
  middleware $ cors (const $ Just corsResourcePolicy)
  get "/" $ text "Welcome to entangle!"
  post "/" $ root `rescue` text
