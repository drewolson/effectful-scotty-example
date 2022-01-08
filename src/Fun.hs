module Fun
  ( main,
  )
where

import Effectful (Eff, IOE, interpret, runEff, type (:>))
import Fun.RequestCounter (RequestCounter (..))
import Fun.Router as Router
import UnliftIO (TVar)
import UnliftIO qualified as UIO
import Web.Scotty.Trans (scottyT)

type Counter = TVar Integer

runRequestCounterIO :: (IOE :> es) => Counter -> Eff (RequestCounter : es) a -> Eff es a
runRequestCounterIO counter = interpret $ const \case
  CurrentCount -> UIO.atomically $ UIO.readTVar counter
  IncrementCount -> UIO.atomically $ UIO.modifyTVar counter (+ 1)

runIO :: Counter -> Eff '[RequestCounter, IOE] a -> IO a
runIO counter = runEff . runRequestCounterIO counter

main :: IO ()
main = do
  counter <- UIO.newTVarIO 0

  scottyT 3000 (runIO counter) Router.router
