module Bench where

import Prelude

import Benchmark (fnEff, runBench)
import Concur.Core (Widget(..), WidgetF(..), awaitViewAction, display, orr)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (resume)
import Control.Monad.ST (ST)
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Foldable (for_)

runToFirstRender :: forall v eff a. Widget v eff a -> Eff eff Unit
runToFirstRender (Widget widget) =
  case resume widget of
    Right _ -> pure unit
    Left (Effect eff) -> do
       w <- eff
       runToFirstRender (Widget w)
    Left (Display v _) -> pure unit

renderAllWith :: forall eff a. (String -> Widget (Array String) eff a) -> Array String -> Widget (Array String) eff a
renderAllWith f = orr <<< map f

main :: forall eff s. Eff (st :: ST s | eff) Unit
main = do
  let bench name f =
        runBench $ do
          for_ [5,10,15,20,25] $ \n ->
            fnEff (name <> " " <> show n)
              (runToFirstRender $ renderAllWith f $ replicate n "foo")

  bench "liftEff pure" (\text -> liftEff (pure unit) >>= \_ -> display [text])
  bench "awaitViewAction" (\text -> awaitViewAction $ \_ -> [text])
  bench "pure" (\text -> pure unit >>= \_ -> display [text])
  bench "display" (\text -> display [text])
