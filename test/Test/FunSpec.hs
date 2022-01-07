module Test.FunSpec
  ( spec,
  )
where

import Effectful (Eff, IOE, interpret, runEff)
import Fun (RequestCounter (..))
import Fun qualified
import Network.Wai (Application)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Test.Hspec.Wai (get, shouldRespondWith, with)
import Web.Scotty.Trans (scottyAppT)

runRequestCounterPure :: Eff (RequestCounter : es) a -> Eff es a
runRequestCounterPure = interpret $ const \case
  CurrentCount -> pure 1
  IncrementCount -> pure ()

runTest :: Eff '[RequestCounter, IOE] a -> IO a
runTest = runEff . runRequestCounterPure

testApp :: IO Application
testApp = scottyAppT runTest Fun.router

spec :: Spec
spec = parallel do
  describe "router" do
    with testApp do
      it "returns the count" do
        get "/" `shouldRespondWith` "Current count: 1"
