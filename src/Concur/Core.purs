module Concur.Core where

import Prelude

import Concur.Notify (AsyncEff, Channel, await, never, newChannel)
import Control.Alt (class Alt, alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, foldFree, hoistFree, liftF, resume)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (StateT(..), mapStateT)
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, over)

data Notify a

data WidgetF v eff k = Display v (AsyncEff eff k)

derive instance functorWidgetF :: Functor (WidgetF v eff)

newtype Widget v eff a = Widget (Free (WidgetF v eff) a)

derive instance newtypeWidget :: Newtype (Widget v eff a) _
derive newtype instance functorWidget :: Functor (Widget v eff)
derive newtype instance applyWidget :: Apply (Widget v eff)
derive newtype instance applicativeWidget :: Applicative (Widget v eff)
derive newtype instance bindWidget :: Bind (Widget v eff)
derive newtype instance monadWidget :: Monad (Widget v eff)
derive newtype instance monadRecWidget :: MonadRec (Widget v eff)

liftAsyncEff :: forall v eff a. Monoid v => AsyncEff eff a -> Widget v eff a
liftAsyncEff = Widget <<< liftF <<< Display mempty

instance monadEffWidget :: Monoid v => MonadEff eff (Widget v eff) where
  liftEff = liftAsyncEff <<< liftEff

instance altWidget :: Monoid v => Alt (Widget v eff) where
  alt (Widget a) (Widget b) = Widget (altWidgetFree a b)
    where
      altWidgetFree a b =
        case resume a, resume b of
          Right x, _ -> pure x
          _, Right x -> pure x
          Left (Display v1 k1), Left (Display v2 k2) ->
            join $ liftF $ Display (v1 <> v2) $ do
              result <- Left <$> k1 <|> Right <$> k2
              case result of
                Left result1 -> pure (result1 `altWidgetFree` b)
                Right result2 -> pure (a `altWidgetFree` result2)

instance plusWidget :: Monoid v => Plus (Widget v eff) where
  empty = liftAsyncEff empty

instance alternativeWidget :: Monoid v => Alternative (Widget v eff)

awaitViewAction :: forall a v eff. Monoid v => (Channel a -> v) -> Widget v eff a
awaitViewAction f = do
  ch <- liftAsyncEff newChannel
  Widget $ liftF $ Display (f ch) (await ch)

display :: forall v eff a. v -> Widget v eff a
display v = Widget $ liftF $ Display v never

mapViewWidget :: forall v v' eff. (v -> v') -> Widget v eff ~> Widget v' eff
mapViewWidget f = over Widget (hoistFree (\(Display v k) -> Display (f v) k))

runWidgetWith :: forall v eff a. (v -> AsyncEff eff Unit) -> Widget v eff a -> AsyncEff eff a
runWidgetWith onViewChange (Widget w) = foldFree f w
  where
    f :: forall b. WidgetF v eff b -> AsyncEff eff b
    f (Display v k) = do
      onViewChange v
      k

class MonadView v m | m -> v where
  mapView :: (v -> v) -> m ~> m

instance monadViewWidget :: MonadView v (Widget v eff) where
  mapView = mapViewWidget

instance monadViewStateT :: MonadView v m => MonadView v (StateT s m) where
  mapView f = mapStateT (mapView f)

orr :: forall m a. Alternative m => Array (m a) -> m a
orr = foldr alt empty
