module Main where

import Prelude

import Concur.Core (Widget)
import Concur.Notify (runAsyncEff)
import Concur.React (HTML, el, input, runWidget, text)
import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Node)

foreign import getElementById :: forall eff. String -> Eff eff Node

mainWidget :: forall eff a. Widget HTML eff a
mainWidget =
  el "div" {}
    [ text "Hello!"
    , (fix $ \loop value ->
        (input value <|> text value) >>= loop) "Foo"
    ]

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  root <- getElementById "app"
  runAsyncEff (runWidget root mainWidget) pure
