module Concur.Core where

import Prelude

import Concur.Channel (Awaits, Channel, await, newChannel, runAwaits)
import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, foldFree, hoistFree, liftF, resume)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (StateT, mapStateT)
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, over)

data Notify a

data WidgetF v eff k = Effect (Eff eff k) | Display v (Awaits k)

derive instance functorWidgetF :: Functor (WidgetF v eff)

newtype Widget v eff a = Widget (Free (WidgetF v eff) a)

derive instance newtypeWidget :: Newtype (Widget v eff a) _
derive newtype instance functorWidget :: Functor (Widget v eff)
derive newtype instance applyWidget :: Apply (Widget v eff)
derive newtype instance applicativeWidget :: Applicative (Widget v eff)
derive newtype instance bindWidget :: Bind (Widget v eff)
derive newtype instance monadWidget :: Monad (Widget v eff)
derive newtype instance monadRecWidget :: MonadRec (Widget v eff)

instance monadEffWidget :: Monoid v => MonadEff eff (Widget v eff) where
  liftEff eff = Widget $ liftF $ Effect eff

instance altWidget :: Monoid v => Alt (Widget v eff) where
  alt (Widget a) (Widget b) = Widget (altWidgetFree a b)
    where
      altWidgetFree a b =
        case resume a, resume b of
          Right x, _ ->
            pure x

          _, Right x ->
            pure x

          Left (Effect eff), _ ->
            join $ liftF $ Effect (map (\l -> l `altWidgetFree` b) eff)

          _, Left (Effect eff) ->
            join $ liftF $ Effect (map (\r -> a `altWidgetFree` r) eff)

          Left (Display v1 k1), Left (Display v2 k2) ->
            join $ liftF $ Display (v1 <> v2) $
              map (\result1 -> result1 `altWidgetFree` b) k1 <>
              map (\result2 -> a `altWidgetFree` result2) k2

instance plusWidget :: Monoid v => Plus (Widget v eff) where
  empty = display mempty

instance alternativeWidget :: Monoid v => Alternative (Widget v eff)

awaitViewAction :: forall a v eff. Monoid v => (Channel a -> v) -> Widget v eff a
awaitViewAction f = do
  ch <- liftEff newChannel
  Widget $ liftF $ Display (f ch) (await ch)

display :: forall v eff a. v -> Widget v eff a
display v = Widget $ liftF $ Display v mempty

liftAwaits :: forall v eff a. Monoid v => Awaits a -> Widget v eff a
liftAwaits awaits = Widget $ liftF $ Display mempty awaits

mapViewWidget :: forall v v' eff. (v -> v') -> Widget v eff ~> Widget v' eff
mapViewWidget f = over Widget (hoistFree (mapViewWidgetF f))

mapViewWidgetF :: forall v v' eff. (v -> v') -> WidgetF v eff ~> WidgetF v' eff
mapViewWidgetF f (Effect eff) = Effect eff
mapViewWidgetF f (Display v k) = Display (f v) k

runWidgetWith :: forall v eff a. (v -> Aff eff Unit) -> Widget v eff a -> Aff eff a
runWidgetWith onViewChange (Widget w) = foldFree f w
  where
    f :: WidgetF v eff ~> Aff eff

    f (Effect eff) = liftEff eff

    f (Display v awaits) = do
      onViewChange v
      makeAff $ \k -> do
        runAwaits awaits (k <<< Right)
        pure nonCanceler

class MonadView v m | m -> v where
  mapView :: (v -> v) -> m ~> m

instance monadViewWidget :: MonadView v (Widget v eff) where
  mapView = mapViewWidget

instance monadViewStateT :: MonadView v m => MonadView v (StateT s m) where
  mapView f = mapStateT (mapView f)

orr :: forall m a. Alternative m => Array (m a) -> m a
orr = foldr alt empty
