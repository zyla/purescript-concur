module Concur.Notify where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)

newtype AsyncEff eff a = AsyncEff ((a -> Eff eff Unit) -> Eff eff Unit)

runAsyncEff :: forall eff a. AsyncEff eff a -> (a -> Eff eff Unit) -> Eff eff Unit
runAsyncEff (AsyncEff eff) = eff

-- | Start an AsyncEff computation, and don't get notification when it finishes.
fireAsyncEff :: forall eff. AsyncEff eff Unit -> Eff eff Unit
fireAsyncEff action = runAsyncEff action pure

instance functorAsyncEff :: Functor (AsyncEff eff) where
  map f (AsyncEff eff) = AsyncEff (\cont -> eff (f >>> cont))

instance applyAsyncEff :: Apply (AsyncEff eff) where
  apply (AsyncEff mf) (AsyncEff mx) =
    AsyncEff $ \k ->
      mf $ \f ->
        mx $ \x ->
          k (f x)

instance applicativeAsyncEff :: Applicative (AsyncEff eff) where
  pure x = AsyncEff $ \k -> k x

instance bindAsyncEff :: Bind (AsyncEff eff) where
  bind (AsyncEff mx) y =
    AsyncEff $ \k ->
      mx $ \x ->
        runAsyncEff (y x) k

instance monadAsyncEff :: Monad (AsyncEff eff)

instance monadRecAsyncEff :: MonadRec (AsyncEff eff) where
  tailRecM f x = do
    step <- f x
    case step of
      Done y -> pure y
      Loop x' -> tailRecM f x'

instance monadEffAsyncEff :: MonadEff eff (AsyncEff eff) where
  liftEff eff = AsyncEff $ \k -> eff >>= k

foreign import data Channel :: Type -> Type

foreign import newChannel :: forall eff a. AsyncEff eff (Channel a)

foreign import await :: forall eff a. Channel a -> AsyncEff eff a

foreign import yield :: forall eff a. Channel a -> a -> AsyncEff eff Unit

-- | A computation that never returns.
never :: forall eff a. AsyncEff eff a
never = AsyncEff $ \_ -> pure unit

-- | Run two computations in parallel and return the result of whichever finished first.
foreign import race :: forall eff a. AsyncEff eff a -> AsyncEff eff a -> AsyncEff eff a

foreign import race2 :: forall eff a
   . AsyncEff eff a
  -> AsyncEff eff a
  -> AsyncEff eff { left :: Boolean, winning :: a, losing :: AsyncEff eff a }

instance altAsyncEff :: Alt (AsyncEff eff) where
  alt = race

instance plusAsyncEff :: Plus (AsyncEff eff) where
  empty = never

instance alternativeAsyncEff :: Alternative (AsyncEff eff)
