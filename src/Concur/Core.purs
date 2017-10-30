module Concur.Core where

import Prelude

import Concur.Notify (AsyncEff, Channel, await, never, newChannel)
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Applicative (unless)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, hoistFree, liftF, resume)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, over, under)

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

mapView :: forall v v' eff a. (v -> v') -> Widget v eff a -> Widget v' eff a
mapView f = over Widget (hoistFree (\(Display v k) -> Display (f v) k))
