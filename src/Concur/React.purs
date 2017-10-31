module Concur.React where

import Prelude

import Concur.Core (Widget, awaitViewAction, display, liftAsyncEff, mapView, runWidgetWith)
import Concur.Notify (AsyncEff, Channel, newChannel, runAsyncEff, yield)
import Control.Alternative (alt, empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import DOM (DOM)
import DOM.HTML.HTMLTextAreaElement (validity)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, Node)
import Data.Foldable (foldr)
import Data.Foreign (renderForeignError)
import Data.Maybe (Maybe(..))
import Data.Ref (Ref, newRef, readRef, writeRef)
import React (Event, ReactElement, createElementTagName, createElementTagNameDynamic)
import React.DOM as R
import React.DOM.Props (unsafeFromPropsArray)
import React.DOM.Props as P
import ReactDOM (render)
import Unsafe.Coerce (unsafeCoerce)

type HTML = Array ReactElement

runWidget :: forall eff a. Node -> Widget HTML (dom :: DOM | eff) a -> AsyncEff (dom :: DOM | eff) a
runWidget root widget = do
  let viewChanged elements = liftEff $ do
        _ <- render (createElementTagName "div" {} elements) (unsafeNodeToElement root)
        pure unit

  runWidgetWith viewChanged widget

unsafeNodeToElement :: Node -> Element
unsafeNodeToElement = unsafeCoerce

el :: forall props eff a
   . String
  -> { | props }
  -> Array (Widget HTML eff a)
  -> Widget HTML eff a
el tag props =
  mapView (\elems -> [createElementTagName tag props elems])
    <<< foldr alt empty

text :: forall eff a. String -> Widget HTML eff a
text t = display [R.text t]

input :: forall eff. String -> Widget HTML eff String
input value =
  awaitViewAction $ \channel ->
    [createElementTagName "input"
      (unsafeFromPropsArray
         [ P.onChange $ \event -> do
             runAsyncEff (yield channel (unsafeTargetValue event)) pure
         , P.value value
         ]
      )
      []]

unsafeTargetValue :: Event -> String
unsafeTargetValue event = (unsafeCoerce event).target.value
