module Main where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Reflex.Dom.Core
import qualified Reflex.Dom.Main as Main
import Rememorate.Run (run)

main :: IO ()
main =
  run 3003 $ Main.mainWidgetWithHead headWidget bodyWidget

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text "rememorate"

bodyWidget ::
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) =>
  m ()
bodyWidget = do
  el "h1" $ text "rememorate"
  clicked <- stoneButton
  cnt <- foldDyn (+) (0 :: Int) $ 1 <$ clicked
  elClass "p" "result" $ do
    dyn_ $
      ffor cnt $ \case
        0 -> text "Go ahead and hit the stone"
        n -> do
          text $ T.pack (show n)
          text " heads!"
  divClass "footer" $ do
    elAttr "a" ("href" =: homePage) $
      text "View source on GitHub"
  where
    homePage = "https://github.com/srid/rememorate"

stoneButton :: DomBuilder t m => m (Event t ())
stoneButton = do
  let attr = "style" =: "font-size: 200%;"
  clickEvent $ elAttr' "button" attr stone

stone :: DomBuilder t m => m ()
stone =
  text "🗿"

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent =
  fmap (void . domEvent Click . fst)
