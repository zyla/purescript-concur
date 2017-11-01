module Main where

import Prelude

import Concur.Channel (yield)
import Concur.Core (Widget, awaitAction, orr)
import Concur.React (HTML, button, checkbox, el, inputOnEnter, runWidget, text, unsafeTargetValue)
import Control.Alternative (class Alternative, empty)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId), documentToNonElementParentNode)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import React.DOM.Props as P

type Todo =
  { completed :: Boolean
  , text :: String
  }

type TodoList = Array Todo

initialTodos :: Array Todo
initialTodos =
  [ { completed: true, text: "Taste JavaScript" }
  , { completed: false, text: "Buy a unicorn" }
  ]

stateful :: forall s m a. Monad m => (s -> m s) -> s -> m a
stateful fn initialState = fn initialState >>= stateful fn

mainWidget :: forall eff a. Widget HTML eff a
mainWidget = stateful todoList initialTodos

processArray :: forall m a. Alternative m => (a -> m (Maybe a)) -> Array a -> m (Array a)
processArray f array = map updateArray $ orr $ mapWithIndex (\index value -> Tuple index <$> f value) array
  where
    updateArray (Tuple index Nothing) = 
      fromMaybe array $ Array.deleteAt index array
    updateArray (Tuple index (Just value)) = 
      fromMaybe array $ Array.updateAt index value array

todoList :: forall eff. TodoList -> Widget HTML eff TodoList
todoList todos =
  el "section" [ P.className "todoapp" ]
    [ el "header" [ P.className "header" ]
        [ el "h1" [] [ text "todos" ]
        , inputOnEnter ""
            [ P.className "new-todo"
            , P.placeholder "What needs to be done?"
            , P.autoFocus true
            ]
            >>= \newText ->
                let newTodo = { completed: false, text: newText }
                in pure (todos <> [newTodo])
        ]
    , el "section" [ P.className "main" ]
        [ el "input"
            [ P._id "toggle-all"
            , P.className "toggle-all"
            , P._type "checkbox"
            ] []
        , el "label" [ P.htmlFor "toggle-all" ]
            [ text "Mark all as complete" ]
        , el "ul" [ P.className "todo-list" ]
            [ processArray todoItemWidget todos
            ]
        ]
    ]

-- | A Widget that displays a Todo item.
-- Returns when editing is finished. Returns Nothing if the todo is to be destroyed.
todoItemWidget :: forall eff. Todo -> Widget HTML eff (Maybe Todo)
todoItemWidget = go false
  where

  go editing todo = do
    action <- el "li"
      [ P.className $
          (if todo.completed then " completed" else "") <>
          (if editing then " editing" else "")
      ]
      [ el "div" [ P.className "view" ]
          [ checkbox todo.completed
              [ P.className "toggle"
              , P.checked todo.completed
              ]
              >>= \completed ->
                  pure $ Right $ Just $ todo { completed = completed }

          , if not editing
              then do
                awaitAction $ \channel ->
                  el "label"
                    [ P.onDoubleClick $ \_ ->
                        unless todo.completed $
                          yield channel unit
                    ]
                    [ text todo.text ]
                pure $ Left true
              else
                empty

          , button [ P.className "destroy" ] []
              *> pure (Right Nothing)

          ]
      , if editing
          then do
            newText <- awaitAction $ \channel ->
              inputOnEnter todo.text
                [ P.className "edit"
                , P.autoFocus true
                , P.onBlur $ \event -> yield channel (unsafeTargetValue event)
                ]
            pure $ Right $ Just $ todo { text = newText }
          else
            empty
      ]

    case action of
      Left editing' ->
        go editing' todo
      Right newTodo ->
        pure newTodo


getElementByIdOrDie :: forall eff. String -> Eff (dom :: DOM | eff) Element
getElementByIdOrDie id = do
  doc <- documentToNonElementParentNode <$> htmlDocumentToDocument <$> (window >>= document)
  m_elem <- getElementById (ElementId id) doc
  unsafePartial (case m_elem of
                   Just element -> pure element)


main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
  root <- getElementByIdOrDie "app"
  launchAff_ $ runWidget root mainWidget
  log "Started"
