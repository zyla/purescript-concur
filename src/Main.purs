module Main where

import Prelude

import Concur.Core (Widget(..), WidgetF(..), display, liftAsyncEff)
import Concur.Notify (AsyncEff, Channel, await, newChannel, runAsyncEff, yield)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (foldFree)
import Control.Monad.Rec.Class (forever)
import Data.Foldable (traverse_)
import Unsafe.Coerce (unsafeCoerce)

type View = Array String

text :: forall t1 t2 t3. t1 -> Widget (Array t1) t3 t2
text t = display [t]

widget :: forall eff. Channel String -> Widget View (console :: CONSOLE | eff) Unit
widget kbd =
  text "Hello"
  <|>
  textInput kbd
  <|>
  text "Bar"

textInput :: forall eff. Channel String -> Widget View (console :: CONSOLE | eff) Unit
textInput kbd = do
  value <- liftAsyncEff $ await kbd
  go value

 where
   go value = do
    value' <- text ("Input from keyboard: " <> value) <|> liftAsyncEff (await kbd)
    go value'

runWidgetText :: forall eff a. Widget View (console :: CONSOLE | eff) a -> AsyncEff (console :: CONSOLE | eff) a
runWidgetText (Widget w) = foldFree f w
  where
    f :: forall b. WidgetF View (console :: CONSOLE | eff) b -> AsyncEff (console :: CONSOLE | eff) b
    f (Display v k) = do
      liftEff $ do
        log "NEW VIEW:"
        log "-------------------------"
        traverse_ log v
        log "-------------------------"
      k

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runAsyncEff asyncMain pure

asyncMain :: forall eff. AsyncEff (console :: CONSOLE | eff) Unit
asyncMain = do
  kbd <- newChannel
  handleInput kbd
  runWidgetText (widget kbd)
  liftEff (log "program finished")

handleInput kbd = liftEff $
                     onStdin $ \s ->
                       runAsyncEff (yield kbd s) pure

foreign import onStdin :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
