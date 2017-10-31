module Data.Ref where

import Prelude

import Control.Monad.Eff (Eff)

foreign import data Ref :: Type -> Type

foreign import newRef :: forall eff a. a -> Eff eff (Ref a)

foreign import readRef :: forall eff a. Ref a -> Eff eff a
foreign import writeRef :: forall eff a. Ref a -> a -> Eff eff Unit
