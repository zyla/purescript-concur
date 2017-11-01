module Test1 where

import Prelude

import Concur.Core (Widget(..), display, liftAsyncEff, orr)
import Concur.Notify (AsyncEff(..), await, fireAsyncEff, never, newChannel, yield)
import Control.Alternative (empty, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import Data.Array (elem)
import Data.Foldable (for_)
import Data.String (singleton, toCharArray)
import NodeMain (View, handleInput, onStdin, runWidgetText)

main :: Eff (console :: CONSOLE) Unit
main = fireAsyncEff (runWidgetText mainWidget)

mainWidget :: Widget View (console :: CONSOLE) Unit
mainWidget = do
  chA <- liftAsyncEff newChannel
  chB <- liftAsyncEff newChannel
  liftEff $ onStdin $ \str ->
    for_ (toCharArray str) $ \c -> do
      log ("STDIN: " <> show c)
      if c `elem` toCharArray "qwertyuiop"
        then fireAsyncEff (yield chA c)
        else if c `elem` toCharArray "asdfghjkl"
          then fireAsyncEff (yield chB c)
          else pure unit

  let chCounter ch name str = do
        c <- display [name <> ": " <> show str] <|> liftAsyncEff (await ch)
        liftEff $ log ("received " <> show c <> " for " <> name)
        chCounter ch name (str <> singleton c)

  orr
    [ chCounter chA "A" ""
    , chCounter chB "B" ""
    ]
