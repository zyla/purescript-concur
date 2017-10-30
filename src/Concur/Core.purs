module Concur.Core where

import Prelude

import Concur.Notify (AsyncEff)
import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, resume)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)

data Notify a

data WidgetF v eff k = Display v (AsyncEff eff k)

derive instance functorWidgetF :: Functor (WidgetF v eff)

newtype Widget v eff a = Widget (Free (WidgetF v eff) a)

derive newtype instance functorWidget :: Functor (Widget v eff)
derive newtype instance applyWidget :: Apply (Widget v eff)
derive newtype instance applicativeWidget :: Applicative (Widget v eff)
derive newtype instance bindWidget :: Bind (Widget v eff)
derive newtype instance monadWidget :: Monad (Widget v eff)

liftAsyncEff :: forall v eff a. Monoid v => AsyncEff eff a -> Widget v eff a
liftAsyncEff = Widget <<< liftF <<< Display mempty

instance monadEffWidget :: Monoid v => MonadEff eff (Widget v eff) where
  liftEff = liftAsyncEff <<< liftEff

instance altWidget :: Monoid v => Alt (Widget v eff) where
  alt (Widget a) (Widget b) =
    case resume a, resume b of
      Right x, _ -> pure x
      _, Right x -> pure x
      Left (Display v1 k1), Left (Display v2 k2) ->
        Widget $ liftF $ Display (v1 <> v2) ?vae --(v1 <> v2) (k1 <|> k2)
