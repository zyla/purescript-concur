module Concur.Channel (
    Awaits
  , Channel
  , newChannel
  , yield
  , await
  , runAwaits
) where

import Prelude

import Concur.UniqueMap (UniqueMap)
import Concur.UniqueMap as UM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Array (uncons)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)

newtype IO a = IO (forall eff. Eff eff a)
instance functorIO :: Functor IO where
  map f (IO eff) = IO (map f eff)

unsafeRunIO :: forall eff a. IO a -> Eff eff a
unsafeRunIO (IO eff) = eff

liftEffIO :: forall eff a. Eff eff a -> IO a
liftEffIO = IO <<< unsafeCoerceEff

data Channel a = Channel (Ref (ChannelState a))

type ChannelState a =
  { items :: Array a
  , listeners :: UniqueMap (a -> IO Unit)
  }

newtype Await r = Await (Exists (AwaitF r))

instance functorAwait :: Functor Await where
  map f (Await ex) =
    Await (runExists (\(AwaitF chan k) -> mkExists (AwaitF chan (f <<< k))) ex)

data AwaitF r a = AwaitF (Channel a) (a -> r)

newtype Awaits a = Awaits (Array (Await a))

derive instance newtypeAwaits :: Newtype (Awaits a) _
derive instance functorAwaits :: Functor Awaits
derive newtype instance semigroupAwaits :: Semigroup (Awaits a)
derive newtype instance monoidAwaits :: Monoid (Awaits a)

await :: forall a. Channel a -> Awaits a
await chan = Awaits [Await (mkExists (AwaitF chan id))]

newChannel :: forall eff a. Eff eff (Channel a)
newChannel = unsafeRunRef $ Channel <$> newRef { items: [], listeners: UM.empty }

yield :: forall eff a. Channel a -> a -> Eff eff Unit
yield (Channel ref) item = unsafeRunRef $ do
  state <- readRef ref
  case UM.deleteAny state.listeners of
    Just { value: listener, map } -> do
      writeRef ref $ state { listeners = map }
      unsafeRunIO $ listener item
    Nothing ->
      writeRef ref $ state { items = state.items <> [item] }

-- | If the channel waited on has items, take the first item
-- and return the value. Else no change to channel state.
takeIfFull :: forall eff a. Await a -> Eff eff (Maybe a)
takeIfFull (Await ex) = unsafeRunRef $
  runExists (\(AwaitF (Channel ref) k) -> do
    state <- readRef ref
    case uncons state.items of
      Just { head, tail } -> do
        writeRef ref $ state { items = tail }
        pure (Just (k head))
      Nothing ->
        pure Nothing
  ) ex

firstM :: forall m a b. Monad m => (a -> m (Maybe b)) -> Array a -> m (Maybe b)
firstM f xs =
  case uncons xs of

    Just { head, tail } -> do
      result <- f head
      case result of
        Just x -> pure (Just x)
        Nothing -> firstM f tail

    Nothing ->
      pure Nothing

runAwaits :: forall eff a. Awaits a -> (a -> Eff eff Unit) -> Eff eff Unit
runAwaits (Awaits awaits) cont = unsafeRunRef $ do
  m_item <- firstM takeIfFull awaits
  case m_item of

    Just item ->
      unsafeCoerceEff $ cont item

    Nothing -> do
      key <- UM.newUnique

      let finish value = do
            for_ awaits $ \(Await ex) ->
              runExists (\(AwaitF (Channel ref) _) -> do
                state <- readRef ref
                writeRef ref $ state { listeners = UM.delete key state.listeners }
              ) ex
            unsafeCoerceEff $ cont value
      
      for_ awaits $ \(Await ex) ->
        runExists (\(AwaitF (Channel ref) k) -> do
          state <- readRef ref
          writeRef ref $
            state
              { listeners = UM.insert key (liftEffIO <<< finish <<< k) state.listeners }
        ) ex
