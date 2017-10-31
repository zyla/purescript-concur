module Concur.React where

import Prelude

import Concur.Core (class MonadView, Widget, awaitViewAction, display, liftAsyncEff, mapView, orr, runWidgetWith)
import Concur.Notify (AsyncEff, Channel, await, newChannel, runAsyncEff, yield)
import Control.Alt (alt)
import Control.Alternative (class Alternative, alt, empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.State (StateT(..), mapStateT)
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
import React.DOM.Props (Props, unsafeFromPropsArray)
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

el :: forall a m
   . MonadView HTML m
  => Alternative m
  => String
  -> Array Props
  -> Array (m a)
  -> m a
el tag props =
  orr >>>
  mapView (\elems -> [createElementTagName tag (unsafeFromPropsArray props) elems])

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

-- | A button widget. Returns when pressed.
button :: forall eff. Array Props -> Array (Widget HTML eff Unit) -> Widget HTML eff Unit
button props children = do
  channel <- liftAsyncEff newChannel
  let handleClick _event = runAsyncEff (yield channel unit) pure
  el "button" (props <> [P.onClick handleClick]) $
    [ liftAsyncEff (await channel) ] <>
    children
