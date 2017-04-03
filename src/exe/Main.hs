{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.Traversable

import           Language.Haskell.Interpreter hiding (get)
import           System.IO
import           System.IO.Temp

import           Data.Aeson.Types
import qualified Data.ByteString
import           Data.ByteString.Lazy         (fromStrict)
import           Data.FileEmbed
import           Data.Monoid                  ((<>))
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

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left a)  = Left $ f a
mapLeft _ (Right r) = Right r

useHint :: String -> [String] -> ActionM (Either String [String])
useHint input exprs =
  let
    go :: IO (Either String [String])
    go =
      withSystemTempFile "Interpreted.hs" useFile
    useFile :: FilePath -> Handle -> IO (Either String [String])
    useFile path handle = do
      hPutStrLn handle "module Interpreted where"
      hPutStrLn handle ""
      let imports = [ "Prelude", "Quipper"]
      forM_ imports $ \i -> hPutStrLn handle ("import " ++ i)
      hPutStrLn handle ""
      hPutStrLn handle "reset_at :: Qubit -> Circ ()"
      hPutStrLn handle "reset_at = named_gate_at \"RESET\""
      hPutStrLn handle ""
      hPutStr handle input
      hClose handle
      putStrLn "==="
      putStrLn path
      readFile path >>= putStrLn
      putStrLn "==="
      result <- runInterpreter $ do
        loadModules [path]
        setTopLevelModules ["Interpreted"]
        setImports
          [ "Prelude"
          , "Quipper"
          , "Transitions"
          , "Qpmc"
          , "Expr"
          , "SymbolicMatrix"
          , "Data.Matrix"
          ]
        forM exprs (`interpret` (as :: String))
      return $ mapLeft errorString result
  in
    liftAndCatchIO go

root :: ActionM ()
root = do
  request <- jsonData :: ActionM Request
  let name = rName request
  let code = rCode request
  let type_ = rType request
  let treeCode = "either (\"Error: \" ++) show $ circToTree " ++ name
  let final = if rRecursive request then "recursive" else "nonrecursive"
  let kind = rKind request
  let qpmcCode = concat ["either (\"Error: \" ++) (\\cm -> toQpmc (", show name, ", cm)) $ circMatrices ", final, " ", kind, " ",  name]
  r <- useHint code [treeCode, qpmcCode]
  json $ case r of
    Right [tree, qpmc] -> Response qpmc tree
    Left err           -> Response err err

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
main = do
  putStrLn "You can see the interface at http://localhost:3113/"
  scotty 3113 handler

dev :: (Application -> IO a) -> IO a
dev h = scottyApp handler >>= h

indexHtml :: Data.ByteString.ByteString
indexHtml = $(embedFile "src/exe/index.html")

elmJs :: Data.ByteString.ByteString
elmJs = $(embedFile "src/exe/elm.js")

styleCss :: Data.ByteString.ByteString
styleCss = $(embedFile "src/exe/style.css")

favicons :: [(FilePath, Data.ByteString.ByteString)]
favicons = $(embedDir "src/exe/favicons")

rawStrict :: Data.ByteString.ByteString -> ActionM ()
rawStrict = raw . fromStrict

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x y = reverse x `startsWith` reverse y

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith x y = and $ zipWith (==) x y

handler :: ScottyM ()
handler = do
  middleware $ cors (const $ Just corsResourcePolicy)
  get "/" . rawStrict $ indexHtml
  get "/elm.js" . rawStrict $ elmJs
  get "/style.css" . rawStrict $ styleCss
  post "/" $ root `rescue` text
  get "/:path" $ do
    path <- param "path"
    case [fstring | (fpath, fstring) <- favicons,  fpath `endsWith` path] of
      []    -> next
      (x:_) -> rawStrict x
