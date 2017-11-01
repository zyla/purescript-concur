module Concur.VDom where

import Prelude

import Concur.Core (Widget, awaitViewAction, display, mapView, runWidgetWith)
import Concur.Notify (AsyncEff, runAsyncEff)
import Concur.Awaits (Channel, newChannel, yield)
import Control.Alternative (alt, empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import DOM (DOM)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Node)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Ref (Ref, newRef, readRef, writeRef)
import VirtualDOM (createElement, diff, patch)
import VirtualDOM.VTree (VHook, VTree, vhook, vnode, vtext)

type HTML = Array VTree

runWidget :: forall eff a. Node -> Widget HTML (dom :: DOM | eff) a -> AsyncEff (dom :: DOM | eff) a
runWidget root widget = do
  treeRef :: Ref (Maybe { node :: Node, view :: VTree }) <- liftEff $ newRef Nothing

  let viewChanged nodes = liftEff $ do
        let newView = vnode "div" {} nodes
        m_oldRef <- readRef treeRef
        case m_oldRef of

          Nothing -> do
            let node = createElement newView
            _ <- appendChild node root
            writeRef treeRef (Just { node: node, view: newView })

          Just { node: node, view: oldView } -> do
            _ <- patch (diff oldView newView) node
            writeRef treeRef (Just { node: node, view: newView })

  runWidgetWith viewChanged widget

el :: forall props eff a
   . String
  -> { | props }
  -> Array (Widget HTML eff a)
  -> Widget HTML eff a
el tag props = mapView (\elems -> [vnode tag props elems]) <<< foldr alt empty

text :: forall eff a. String -> Widget HTML eff a
text t = display [vtext t]

input :: forall eff. String -> Widget HTML eff String
input value =
  awaitViewAction $ \channel ->
    [vnode "input"
      { "oninput-hook": mkHook (onInputHandler channel)
      , "value": value
      } []]

onInputHandler :: forall eff. Channel String -> Node -> Eff eff Unit
onInputHandler channel node = do
  setEventHandler node "oninput" (do
    value <- getValue node
    yield channel value
    )

foreign import setEventHandler :: forall eff. Node -> String -> Eff eff Unit -> Eff eff Unit
foreign import getValue :: forall eff. Node -> Eff eff String

mkHook :: forall eff. (Node -> Eff eff Unit) -> VHook
mkHook hook = vhook { hook: mkEffFn1 hook }
