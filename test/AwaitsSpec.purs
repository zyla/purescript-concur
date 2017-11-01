module AwaitsSpec where

import Prelude

import Concur.Awaits (Awaits, await, newChannel, runAwaits, yield)
import Control.Monad.Aff (Aff, forkAff, joinFiber, makeAff, nonCanceler)
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List.Lazy as LL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Awaits" $ do

  it "single channel, 1 element present" $ do
    chan <- liftEff newChannel
    liftEff $ yield chan "foo"
    value <- runAwaitsAff (await chan)
    value `shouldEqual` "foo"

  it "single channel, 2 elements present" $ do
    chan <- liftEff newChannel
    liftEff $ yield chan "foo"
    liftEff $ yield chan "bar"

    value <- runAwaitsAff (await chan)
    value `shouldEqual` "foo"

    value <- runAwaitsAff (await chan)
    value `shouldEqual` "bar"

  it "single channel, 2 elements listened" $ do
    chan <- liftEff newChannel

    fiber <- forkAff $ replicateM 2 $ runAwaitsAff (await chan)

    liftEff $ yield chan "foo"
    liftEff $ yield chan "bar"

    values <- joinFiber fiber
    values `shouldEqual` ["foo", "bar"]

  it "two channels, elements present" $ do
    chan1 <- liftEff newChannel
    chan2 <- liftEff newChannel

    liftEff $ yield chan1 "foo"
    liftEff $ yield chan2 "bar"

    value1 <- runAwaitsAff (await chan1 <> await chan2)
    value1 `shouldEqual` "foo"

    value2 <- runAwaitsAff (await chan1 <> await chan2)
    value2 `shouldEqual` "bar"

  it "two channels, elements listened" $ do
    chan1 <- liftEff newChannel
    chan2 <- liftEff newChannel

    fiber <- forkAff $ replicateM 2 $ runAwaitsAff $
      await chan1 <> await chan2

    liftEff $ yield chan1 "foo"
    liftEff $ yield chan2 "bar"

    values <- joinFiber fiber
    values `shouldEqual` ["foo", "bar"]

  it "single channel, two runs, two elements listened" $ do
    chan <- liftEff newChannel

    fiber1 <- forkAff $ runAwaitsAff $ await chan
    fiber2 <- forkAff $ runAwaitsAff $ await chan

    liftEff $ yield chan "000"
    liftEff $ yield chan "001"

    value1 <- joinFiber fiber1
    value2 <- joinFiber fiber2

    Array.sort [value1, value2] `shouldEqual` ["000", "001"]

replicateM :: forall m a. Monad m => Int -> m a -> m (Array a)
replicateM n = map Array.fromFoldable <<< LL.replicateM n

runAwaitsAff :: forall eff a. Awaits a -> Aff eff a
runAwaitsAff awaits = makeAff $ \cont -> do
  runAwaits awaits (\value -> cont (Right value))
  pure nonCanceler
