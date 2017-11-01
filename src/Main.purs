module Main where

import Prelude

import Concur.Channel (await, newChannel, yield)
import Concur.Core (Widget, liftAwaits, orr)
import Concur.React (HTML, button, el, input, runWidget, text, unsafeTargetValue)
import Control.Alternative (empty, (<|>))
import Control.Lazy (fix)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Node.Types (Node)
import Data.Array (filter, head)
import Data.Foldable (for_)
import Data.Functor (voidRight)
import Data.Tuple (Tuple(..))
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
mainWidget =
  el "div" [] [(fix $ \loop val -> input val >>= loop) "field 1"]
  <|>
  el "div" [] [(fix $ \loop val -> input val >>= loop) "field 2"]
  <|>
   (do
     evalStateT advancedForm initialAF
     text "Advanced form finished"
   )
   <|>
  (forever $ do
    form <- evalStateT form initialForm
    orr
      [ text ("First name: " <> form.firstName)
      , text ("Last name: " <> form.lastName)
      , button [] [ text "Next" ]
      ]
   )
   <|>
   empty

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
        , get >>= \form -> lift $ text ("First name: " <> form.firstName)
        , get >>= \form -> lift $ text ("Last name: " <> form.lastName)
        ]
    , el "div" []
        [ lift $ Save <$ button [] [ text "Save" ]
        ]
    ]

  case action of
    Continue -> form
    Save -> getState

data FileState = NotChosen | Chosen String

data Choice = Default | Text String | File FileState

data ChoiceType = CT_Default | CT_Text | CT_File

type AdvancedForm =
  { file1 :: Choice }

l_file1 :: Lens AdvancedForm Choice
l_file1 = lens _.file1 (_ { file1 = _ })

initialAF :: AdvancedForm
initialAF = { file1: Text "text" }

choiceTypeMatches :: ChoiceType -> Choice -> Boolean
choiceTypeMatches =
  case _, _ of
    CT_Default, Default -> true
    CT_Text,    Text _ -> true
    CT_File,    File _ -> true
    _,          _      -> false

zoom :: forall s m a. Monad m => Lens s a -> StateT a m ~> StateT s m
zoom l inner = do
  s <- get
  Tuple result newState <- lift $ runStateT inner (l.get s)
  put (l.set s newState)
  pure result

advancedForm :: forall eff. StateT AdvancedForm (Widget HTML eff) Unit
advancedForm = do
  action <- el "div" []
    [ voidRight Continue $ zoom l_file1 $ do
        choice <- get
        orr
          [ do newChoiceType <- orr
                 [ label "Choice:"
                 , lift $ select
                     { options: [ CT_Default, CT_Text, CT_File ]
                     , toString:
                         case _ of
                           CT_Default -> "Default"
                           CT_Text -> "Text"
                           CT_File -> "File"
                     , isSelected: \option -> choiceTypeMatches option choice
                     }
                 ]
               unless (choiceTypeMatches newChoiceType choice) $ do
                 liftEff $ unsafeCoerceEff $ log "Changing choice type"
                 put $
                   case newChoiceType of
                     CT_Default -> Default
                     CT_Text -> Text ""
                     CT_File -> File NotChosen

          , case choice of
              Default -> empty
              Text text -> do
                newText <- lift $ input text
                liftEff $ unsafeCoerceEff $ log $ "new text: " <> newText
                put (Text newText)
              File _ -> empty
          ]
    , el "div" []
        [ lift $ Save <$ button [] [ text "Save" ]
        ]
    ]

  case action of
    Continue -> advancedForm
    Save -> pure unit

label :: forall s eff a. String -> StateT s (Widget HTML eff) a
label t =
  el "label" [ P.style { display: "block" } ]
    [ lift $ text t ]

select :: forall eff a.
  { options :: Array a
  , toString :: a -> String
  , isSelected :: a -> Boolean
  } -> Widget HTML eff a
select {options,toString,isSelected} = do
  channel <- liftEff newChannel
  orr
    [ el "select"
         [ P.onChange $ \event -> 
              let value = unsafeTargetValue event
              in
              for_ (head (filter (\opt -> toString opt == value) options)) $ \newValue ->
                yield channel newValue
         ] $
        map (\option ->
               el "option"
                 [ P.value (toString option)
                 , P.selected (isSelected option)
                 ]
                 [ text (toString option) ]
            )
            options
    , liftAwaits (await channel)
    ]

type Lens s a = { get :: s -> a, set :: s -> a -> s }

lens :: forall s a. (s -> a) -> (s -> a -> s) -> Lens s a
lens get set = {get,set}

formInput :: forall s eff. Lens s String -> StateT s (Widget HTML eff) Unit
formInput l = do
  value <- gets l.get
  newValue <- lift $ input value
  modify (\s -> l.set s newValue)

getState :: forall s m. Monad m => StateT s m s
getState = get

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  root <- getElementById "app"
  launchAff_ (runWidget root mainWidget)
