module Main where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Language.Javascript.JSaddle (MonadJSM)
import Neuron.Config.Type (Config (Config))
import qualified Neuron.Web.Query.View as V
import Neuron.Web.Route (Route (Route_Search), RouteConfig (RouteConfig), runNeuronWeb)
import qualified Neuron.Web.View as V
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (ZettelError)
import qualified Neuron.Zettelkasten.Zettel as Z
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
  -- TODO: Pull theme from Config bu using `renderRouteBody`
  elAttr "div" ("class" =: "ui text container" <> "id" =: "neuron-theme-default-blue") $ do
    el "h1" $ text "rememorate"
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
                let zs = G.getZettels . fst <$> nDyn
                    -- TODO
                    rcfg = RouteConfig True (\_someR _attrs w -> elAttr "a" ("href" =: "/foo") w) (\_someR -> "/todo")
                void $
                  runNeuronWeb rcfg $
                    simpleList zs $ \zDyn -> do
                      el "li" $
                        dyn_ $
                          ffor zDyn $ \(z :: Z.Zettel) ->
                            V.renderZettelLink Nothing Nothing Nothing z

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
  resp' <- performRequestAsyncWithError $ XhrRequest "GET" "/cache.json" def <$ pb
  let resp = ffor resp' $ first show >=> decodeXhrResponseWithError
  holdDyn Nothing $ Just <$> resp
  where
    decodeXhrResponseWithError :: FromJSON a => XhrResponse -> Either String a
    decodeXhrResponseWithError =
      fromMaybe (Left "Empty response") . sequence
        . traverse (eitherDecode . BL.fromStrict . encodeUtf8)
        . _xhrResponse_responseText
