module Fun
  ( main,
  )
where

import Effectful (Eff, IOE, interpret, runEff, type (:>))
import Effectful.Concurrent.STM (Concurrent, TVar)
import Effectful.Concurrent.STM qualified as STM
import Fun.RequestCounter (RequestCounter (..))
import Fun.Router qualified as Router
import UnliftIO qualified as UIO
import Web.Scotty.Trans (scottyT)

type Counter = TVar Integer

runRequestCounterIO :: (Concurrent :> es) => Counter -> Eff (RequestCounter : es) a -> Eff es a
runRequestCounterIO counter = interpret $ const \case
  CurrentCount -> STM.readTVarIO counter
  IncrementCount -> STM.atomically $ STM.modifyTVar counter (+ 1)

runIO :: Counter -> Eff '[RequestCounter, Concurrent, IOE] a -> IO a
runIO counter = runEff . STM.runConcurrent . runRequestCounterIO counter

main :: IO ()
main = do
  counter <- UIO.newTVarIO 0

  scottyT 3000 (runIO counter) Router.router
