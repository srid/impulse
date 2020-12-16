{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Some (Some, withSome)
import qualified Data.Text as T
import Language.Javascript.JSaddle (MonadJSM)
import Neuron.Config.Type (Config (Config))
import qualified Neuron.Config.Type as Config
import qualified Neuron.Web.Cache.Type as C
import Neuron.Web.Route (Route (Route_Search), RouteConfig (RouteConfig), routeHtmlPath, runNeuronWeb)
import qualified Neuron.Web.Theme as Theme
import qualified Neuron.Web.View as V
import qualified Neuron.Web.ZIndex as ZIndex
import Reflex.Dom.Core
import qualified Reflex.Dom.Main as Main
import Rememorate.Run (run)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam)

main :: IO ()
main =
  run 3003 $ Main.mainWidgetWithHead headWidget bodyWidget

headWidget :: DomBuilder t m => m ()
headWidget = do
  let dummyConfig = Config Nothing Nothing ["markdown"] "1.0" Nothing "No title" "blue" False
  V.renderRouteHead dummyConfig (Route_Search Nothing) "dummy"

-- TODO(before replacing existing z-index/search entirely)
-- - Tag query, and tag perma urls (to not break existing feature)
-- - Statically render z-index in q.html?

bodyWidget ::
  forall t m.
  (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, HasJSContext (Performable m), MonadJSM (Performable m), MonadHold t m, MonadJSM m) =>
  m ()
bodyWidget = do
  let runNeuronGhcjs = runNeuronWeb routeConfigGhcjs
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
              -- TODO: push dynamic inner?
              dyn_ $
                ffor nDyn $ \C.NeuronCache {..} -> do
                  -- - Use `renderRouteBody` which includes actionbar?
                  V.bodyTemplate _neuronCache_neuronVersion _neuronCache_config $ do
                    let neuronTheme = Theme.mkTheme $ Config.theme _neuronCache_config
                    runNeuronGhcjs $ V.actionsNav neuronTheme Nothing Nothing
                    divClass "ui text container" $ do
                      mquery0 <- urlQueryE [queryKey|q|]
                      qDyn <- searchInput mquery0
                      divClass "ui hidden divider" blank
                      let zindex = ZIndex.buildZIndex _neuronCache_graph _neuronCache_errors
                      runNeuronGhcjs $ ZIndex.renderZIndex Theme.Red zindex qDyn

routeConfigGhcjs :: RouteConfig t m
routeConfigGhcjs =
  RouteConfig False (\someR attrs -> elAttr "a" (attrs <> "href" =: someRouteUrl someR)) someRouteUrl
  where
    someRouteUrl :: Some Route -> Text
    someRouteUrl sr = toText $ withSome sr routeHtmlPath

searchInput ::
  (DomBuilder t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadHold t m, MonadFix m) =>
  Maybe Text ->
  m (Dynamic t (Maybe Text))
searchInput mquery0 = do
  divClass "ui fluid icon input search" $ do
    qDyn <-
      fmap value $
        inputElement $
          def
            & initialAttributes .~ ("placeholder" =: "Search here ..." <> "autofocus" =: "")
            & inputElementConfig_initialValue .~ fromMaybe "" mquery0
    V.fa "search icon fas fa-search"
    qSlow <- debounce 0.5 $ updated qDyn
    holdDyn mquery0 $ fmap (\q -> if q == "" then Nothing else Just q) qSlow

-- Return the value for given query key (eg: ?q=???) from the URL location.
urlQueryE :: (MonadJSM m, PostBuild t m) => URI.RText 'URI.QueryKey -> m (Maybe Text)
urlQueryE key = do
  uri <- URI.mkURI @Maybe <$> getLocationUrl
  pure $ getQueryParam key =<< uri

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
