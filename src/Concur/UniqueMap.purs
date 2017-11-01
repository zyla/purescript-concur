module Concur.UniqueMap (
    Unique
  , UniqueMap
  , newUnique
  , empty
  , insert
  , lookup
  , delete
  , deleteAny
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Ref (Ref, newRef, readRef, writeRef)
import Data.StrMap (StrMap)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

newtype UniqueMap a = UniqueMap (StrMap a)

newtype Unique = Unique String

nextUniqueRef :: Ref Int
nextUniqueRef = unsafePerformEff (newRef 0)

newUnique :: forall eff. Eff eff Unique
newUnique = do
  id <- readRef nextUniqueRef
  writeRef nextUniqueRef (id + 1)
  pure (Unique (show id))

empty :: forall a. UniqueMap a
empty = UniqueMap SM.empty

insert :: forall a. Unique -> a -> UniqueMap a -> UniqueMap a
insert (Unique k) v (UniqueMap m) = UniqueMap (SM.insert k v m)

lookup :: forall a. Unique -> UniqueMap a -> Maybe a
lookup (Unique k) (UniqueMap m) = SM.lookup k m

delete :: forall a. Unique -> UniqueMap a -> UniqueMap a
delete (Unique k) (UniqueMap m) = UniqueMap (SM.delete k m)

-- | Take any element from the map, if it's not empty.
deleteAny :: forall a. UniqueMap a -> Maybe { value :: a, map :: UniqueMap a }
deleteAny (UniqueMap m) = do
  key <- Array.head (SM.keys m)
  Tuple value map <- SM.pop key m
  pure { value, map: UniqueMap map }
