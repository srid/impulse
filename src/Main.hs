{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Some (Some, withSome)
import qualified Data.Text as T
import Language.Javascript.JSaddle (MonadJSM)
import Neuron.Config.Type (Config (Config))
import qualified Neuron.Web.Cache.Type as C
import Neuron.Web.Route (Route (Route_Search), RouteConfig (RouteConfig), routeHtmlPath, runNeuronWeb)
import qualified Neuron.Web.Theme as Theme
import qualified Neuron.Web.View as V
import qualified Neuron.Web.ZIndex as ZIndex
import Reflex.Dom.Core
import qualified Reflex.Dom.Main as Main
import Rememorate.Run (run)

main :: IO ()
main =
  run 3003 $ Main.mainWidgetWithHead headWidget bodyWidget

headWidget :: DomBuilder t m => m ()
headWidget = do
  let dummyConfig = Config Nothing Nothing ["markdown"] "1.0" Nothing "No title" "blue" False
  V.renderRouteHead dummyConfig (Route_Search Nothing) "dummy"

-- TODO cache type:
-- - discard surrounding-context in cache (but still use it in cache.json)
-- - store neuron.json in .neuron/output/

bodyWidget ::
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    HasJSContext (Performable m),
    MonadJSM (Performable m),
    MonadHold t m
  ) =>
  m ()
bodyWidget = do
  -- TODO:
  -- - Put neuron.dhall and neuron version in JSON cache
  -- - Use `renderRouteBody`
  let neuronVersion = "0.0"
      dummyConfig = Config Nothing Nothing ["markdown"] "1.0" Nothing "No title" "blue" False
      neuronTheme = Theme.mkTheme "blue"
      runNeuronGhcjs = runNeuronWeb routeConfigGhcjs
  V.bodyTemplate neuronVersion dummyConfig $ do
    runNeuronGhcjs $ V.actionsNav neuronTheme Nothing Nothing
    divClass "ui text container" $ do
      qDyn <- divClass "ui fluid icon input search" $ do
        qDyn <-
          fmap value $
            inputElement $
              def & initialAttributes .~ ("placeholder" =: "Search here ..." <> "autofocus" =: "")
        V.fa "search icon fas fa-search"
        qSlow <- debounce 0.5 $ updated qDyn
        holdDyn Nothing $ fmap (\q -> if q == "" then Nothing else Just q) qSlow
      divClass "ui hidden divider" blank
      mresp <- maybeDyn =<< getCache @C.NeuronCache
      dyn_ $
        ffor mresp $ \case
          Nothing -> text "Loading JSON cache..."
          Just resp -> do
            eresp <- eitherDyn resp
            dyn_ $
              ffor eresp $ \case
                Left errDyn -> do
                  text "ERROR: "
                  dynText $ T.pack <$> errDyn
                Right nDyn -> do
                  let zindexDyn = ffor nDyn $ \C.NeuronCache {..} ->
                        ZIndex.buildZIndex _neuronCache_graph _neuronCache_errors
                  -- TODO: push dynamic inner
                  dyn_ $
                    ffor zindexDyn $ \zindex ->
                      runNeuronGhcjs $ ZIndex.renderZIndex Theme.Red zindex qDyn

routeConfigGhcjs :: RouteConfig t m
routeConfigGhcjs =
  RouteConfig True (\someR attrs w -> elAttr "a" (attrs <> "href" =: someRouteUrl someR) w) someRouteUrl
  where
    someRouteUrl :: Some Route -> Text
    someRouteUrl sr = toText $ withSome sr routeHtmlPath

getCache ::
  forall a t m.
  ( PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    HasJSContext (Performable m),
    MonadJSM (Performable m),
    MonadHold t m,
    FromJSON a
  ) =>
  m (Dynamic t (Maybe (Either String a)))
getCache = do
  pb <- getPostBuild
  resp' <- performRequestAsyncWithError $ XhrRequest "GET" "cache.json" def <$ pb
  let resp = ffor resp' $ first show >=> decodeXhrResponseWithError
  holdDyn Nothing $ Just <$> resp
  where
    decodeXhrResponseWithError :: FromJSON a => XhrResponse -> Either String a
    decodeXhrResponseWithError =
      fromMaybe (Left "Empty response") . sequence
        . traverse (eitherDecode . BL.fromStrict . encodeUtf8)
        . _xhrResponse_responseText
