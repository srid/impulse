{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- WIP Fork of Language.Javascript.JSaddle.WebSockets

-----------------------------------------------------------------------------

-- |
module Rememorate.Run
  ( run,
  )
where

#ifndef ghcjs_HOST_OS
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
    (jsaddleJs,  jsaddleAppWithJsOr, jsaddleOr )
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
#endif

-- | Run the given 'JSM' action as the main entry point.  Either directly
--   in GHCJS or as a Warp server on the given port on GHC.
#ifdef ghcjs_HOST_OS
run :: Int -> IO () -> IO ()
run _port = id
#else
run :: Int -> JSM () -> IO ()
run port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleAppOr
    where
      jsaddleAppOr =
        jsaddleAppWithJsOr (jsaddleJs False) otherApp
      otherApp req sendResponse = do
        -- HACK: to serve neuron's json cache
        resp <- case W.rawPathInfo req of
          "/cache.json" -> do
            s <- readFileLBS "cache.json"
            pure $ W.responseLBS H.status200 [("Content-Type", "text/plain")] s
          _path ->
            pure $ W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden"
        sendResponse resp
#endif