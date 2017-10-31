module Main where

import Prelude

import Concur.Core (class MonadView, Widget, orr)
import Concur.Notify (runAsyncEff)
import Concur.React (HTML, button, el, input, runWidget, text)
import Control.Alternative (empty, (<|>))
import Control.Lazy (fix)
import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT(..), evalStateT, execStateT, get, gets, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Node.Types (Node)
import React.DOM.Props as P

foreign import getElementById :: forall eff. String -> Eff eff Node

type Form =
  { firstName :: String
  , lastName :: String
  }

initialForm :: Form
initialForm =
  { firstName: ""
  , lastName: ""
  }

mainWidget :: forall eff a. Widget HTML eff a
mainWidget = forever $ do
  form <- evalStateT form initialForm
  orr
    [ text ("First name: " <> form.firstName)
    , text ("Last name: " <> form.lastName)
    , button [] [ text "Next" ]
    ]

data Action = Continue | Save

l_firstName :: Lens Form String
l_firstName = lens _.firstName (_ { firstName = _ })

l_lastName :: Lens Form String
l_lastName = lens _.lastName (_ { lastName = _ })

-- | A form to editing the @Form@ data type.
-- Returns when "Save" is clicked.
form :: forall eff. StateT Form (Widget HTML eff) Form
form = do
  action <- el "div" []
    [ pure Continue <* orr
        [ label "First name"
        , formInput l_firstName
        , label "Last name"
        , formInput l_lastName
        ]
    , el "div" []
        [ lift $ Save <$ button [] [ text "Save" ]
        ]
    ]

  case action of
    Continue -> form
    Save -> getState

label :: forall s eff a. String -> StateT s (Widget HTML eff) a
label t =
  el "label" [ P.style { display: "block" } ]
    [ lift $ text t ]

type Lens s a = { get :: s -> a, set :: s -> a -> s }

lens :: forall s a. (s -> a) -> (s -> a -> s) -> Lens s a
lens get set = {get,set}

formInput :: forall label s eff. Lens s String -> StateT s (Widget HTML eff) Unit
formInput l = do
  value <- gets l.get
  newValue <- lift $ input value
  modify (\s -> l.set s newValue)

getState :: forall s m. Monad m => StateT s m s
getState = get

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  root <- getElementById "app"
  runAsyncEff (runWidget root mainWidget) pure
