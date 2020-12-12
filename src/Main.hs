module Main where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Language.Javascript.JSaddle (MonadJSM)
import Neuron.Config.Type (Config (Config))
import Neuron.Web.Route (Route (Route_Search), RouteConfig (RouteConfig), runNeuronWeb)
import qualified Neuron.Web.Theme as Theme
import qualified Neuron.Web.View as V
import qualified Neuron.Web.ZIndex as ZIndex
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (ZettelError)
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

-- TODO:
-- - normalize and expose in neuron:lib
-- - discard surrounding-context in cache (but still use it in cache.json)
-- - store neuron.json in .neuron/output/
type CacheData = (ZettelGraph, Map ZettelID (NonEmpty ZettelError))

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
  -- TODO: Pull these stuff from Config by using `renderRouteBody`
  let rcfg = RouteConfig True (\_someR attrs w -> elAttr "a" ("href" =: "/foo" <> attrs) w) (\_someR -> "/todo")
      neuronVersion = "0.0"
      dummyConfig = Config Nothing Nothing ["markdown"] "1.0" Nothing "No title" "blue" False
      neuronTheme = Theme.mkTheme "blue"
  V.bodyTemplate neuronVersion dummyConfig $ do
    runNeuronWeb rcfg $ V.actionsNav neuronTheme Nothing Nothing
    divClass "ui text container" $ do
      qDyn <- divClass "ui fluid icon input search" $ do
        qDyn <-
          fmap value $
            inputElement $
              def & initialAttributes .~ ("placeholder" =: "Search here ..." <> "autofocus" =: "")
        -- elAttr "input" ("type" =: "text" <> "id" =: "search-input") blank
        V.fa "search icon fas fa-search"
        qSlow <- debounce 0.5 $ updated qDyn
        holdDyn Nothing $ fmap (\q -> if q == "" then Nothing else Just q) qSlow
      divClass "ui hidden divider" blank
      mresp <- maybeDyn =<< getCache @CacheData
      dyn_ $
        ffor mresp $ \case
          Nothing -> text "Loading"
          Just resp -> do
            eresp <- eitherDyn resp
            dyn_ $
              ffor eresp $ \case
                Left errDyn -> do
                  text "ERROR: "
                  dynText $ T.pack <$> errDyn
                Right nDyn -> do
                  let zindexDyn = uncurry ZIndex.buildZIndex <$> nDyn
                  -- TODO: push dynamic inner
                  dyn_ $
                    ffor2 zindexDyn qDyn $ \zindex mq ->
                      runNeuronWeb rcfg $ ZIndex.renderZIndex Theme.Red zindex mq

{- void $
  runNeuronWeb rcfg $
    simpleList zs $ \zDyn -> do
      el "li" $
        dyn_ $
          ffor zDyn $ \(z :: Z.Zettel) ->
            V.renderZettelLink Nothing Nothing Nothing z
-}

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
