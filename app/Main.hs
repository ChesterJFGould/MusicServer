module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.String
import GHC.Char
import Lib
import qualified Network.HTTP.Types.Status as HTTP.Status
import qualified Network.HTTP.Types.Header as HTTP.Header
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Data.Text as Text
import QStore

runQuery :: (IORef DB) -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
runQuery dbR req resp = do
  body <- Wai.strictRequestBody req
  db <- readIORef dbR
  let bodyS :: String = map (chr . fromIntegral) (BS.unpack body)
  let answ = do {
    q <- parseQuery bodyS;
    Just (query q db)
  }
  case answ of
    Nothing -> resp (Wai.responseLBS HTTP.Status.status400 [] "Bad query")
    Just answ -> resp (Wai.responseLBS HTTP.Status.status200 [] (fromString (show answ)))

runTx :: (IORef DB) -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
runTx dbR req resp = do
  body <- Wai.strictRequestBody req
  db <- readIORef dbR
  let bodyS :: String = map (chr . fromIntegral) (BS.unpack body)
  let db' = do {
    tx <- parseTx bodyS;
    transact tx db
  }
  case db' of
    Nothing -> resp (Wai.responseLBS HTTP.Status.status400 [] "Bad transaction")
    Just db'' -> atomicWriteIORef dbR db'' >> resp (Wai.responseLBS HTTP.Status.status200 [] "")

main :: IO ()
main = do
  dbR <- newIORef emptyDB
  Warp.run
    3141
    (\req resp ->
      case (Wai.requestMethod req, Wai.pathInfo req) of
        ("GET", ("music" : path)) -> resp (Wai.responseFile HTTP.Status.status200 [] (Text.unpack (Text.intercalate "/" ("music" : path))) Nothing)
        ("GET", ["index.json"]) -> resp (Wai.responseFile HTTP.Status.status200 [] "index.json" Nothing)
        ("GET", []) -> resp (Wai.responseFile HTTP.Status.status200 [] "website/index.html" Nothing)
        ("POST", ["query"]) -> runQuery dbR req resp
        ("POST", ["transact"]) -> runTx dbR req resp
        _ -> resp (Wai.responseLBS HTTP.Status.status404 [] "404 Not found")
    )
