module Test.Fun.RouterSpec
  ( spec,
  )
where

import Effectful (Eff, IOE, interpret, runEff)
import Fun.RequestCounter (RequestCounter (..))
import Fun.Router qualified as Router
import Network.Wai (Application)
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Wai (get, shouldRespondWith, with)
import Web.Scotty.Trans (scottyAppT)

runRequestCounterPure :: Eff (RequestCounter : es) a -> Eff es a
runRequestCounterPure = interpret $ const \case
  CurrentCount -> pure 1
  IncrementCount -> pure ()

runIO :: Eff '[RequestCounter, IOE] a -> IO a
runIO = runEff . runRequestCounterPure

testApp :: IO Application
testApp = scottyAppT runIO Router.router

spec :: Spec
spec = parallel do
  describe "router" do
    with testApp do
      it "returns the count" do
        get "/" `shouldRespondWith` "Current count: 1"
