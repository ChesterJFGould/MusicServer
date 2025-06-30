module Main (main) where

import Lib
import qualified Network.HTTP.Types.Status as HTTP.Status
import qualified Network.HTTP.Types.Header as HTTP.Header
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Data.Text as Text

main :: IO ()
main =
  Warp.run
    3141
    (\req resp ->
      case Wai.pathInfo req of
        ("music" : path) -> resp (Wai.responseFile HTTP.Status.status200 [] (Text.unpack (Text.intercalate "/" ("music" : path))) Nothing)
        ["index.json"] -> resp (Wai.responseFile HTTP.Status.status200 [] "index.json" Nothing)
        [] -> resp (Wai.responseFile HTTP.Status.status200 [] "website/index.html" Nothing)
        _ -> resp (Wai.responseLBS HTTP.Status.status404 [] "404 Not found")
    )
