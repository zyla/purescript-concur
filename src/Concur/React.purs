module Concur.React where

import Prelude

import Concur.Channel (await, newChannel, yield)
import Concur.Core (class MonadView, Widget, awaitViewAction, display, liftAwaits, mapView, orr, runWidgetWith)
import Control.Alternative (class Alternative)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Node.Types (Element)
import React (Event, ReactElement, createElementTagName)
import React.DOM as R
import React.DOM.Props (Props, unsafeFromPropsArray)
import React.DOM.Props as P
import ReactDOM (render)
import Unsafe.Coerce (unsafeCoerce)

type HTML = Array ReactElement

runWidget :: forall eff a. Element -> Widget HTML (dom :: DOM | eff) a -> Aff (dom :: DOM | eff) a
runWidget root widget = do
  let viewChanged elements = liftEff $ do
        _ <- render (createElementTagName "div" {} elements) root
        pure unit

  runWidgetWith viewChanged widget

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

-- | Input widget, rendering the given value. Returns on each change.
inputOnChange :: forall eff. String -> Array Props -> Widget HTML eff String
inputOnChange value props =
  awaitViewAction $ \channel ->
    [ createElementTagName "input"
        (unsafeFromPropsArray $
           [ P.onChange $ \event -> yield channel (unsafeTargetValue event)
           , P.value value
           ] <> props
        )
        []
    ]

-- | Input widget, with the given initial value. Returns on Enter keypress.
inputOnEnter :: forall eff. String -> Array Props -> Widget HTML eff String
inputOnEnter initialValue props = do
  channel <- liftEff newChannel
  go channel initialValue

  where
  go channel value = orr
    [ do newValue <- inputOnChange value $
           [ P.onKeyDown $ \keyEvent -> do
                 when (keyEvent.key == "Enter") $
                   yield channel (unsafeTargetValue (unsafeCoerce keyEvent))
           ] <> props
         go channel newValue
    , liftAwaits (await channel)
    ]

unsafeTargetValue :: Event -> String
unsafeTargetValue event = (unsafeCoerce event).target.value

unsafeTargetChecked :: Event -> Boolean
unsafeTargetChecked event = (unsafeCoerce event).target.checked

-- | A button widget. Returns when pressed.
button :: forall eff. Array Props -> Array (Widget HTML eff Unit) -> Widget HTML eff Unit
button props children = do
  channel <- liftEff newChannel
  let handleClick _event = yield channel unit
  el "button" (props <> [P.onClick handleClick]) $
    [ liftAwaits (await channel) ] <>
    children

-- | Checkbox, rendering the given value. Returns on each change.
checkbox :: forall eff. Boolean -> Array Props -> Widget HTML eff Boolean
checkbox value props =
  awaitViewAction $ \channel ->
    [ createElementTagName "input"
        (unsafeFromPropsArray $
           [ P.onChange $ \event -> yield channel (unsafeTargetChecked event)
           , P._type "checkbox"
           , P.checked value
           ] <> props
        )
        []
    ]
