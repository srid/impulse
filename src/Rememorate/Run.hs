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
    ( jsaddleApp, jsaddleOr )
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
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp
#endif