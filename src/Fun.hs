module Fun
  ( main,
  )
where

import Effectful (Eff, IOE, interpret, runEff, type (:>))
import Effectful.Reader (Reader)
import Effectful.Reader qualified as Reader
import Fun.RequestCounter (RequestCounter (..))
import Fun.Router as Router
import UnliftIO (TVar)
import UnliftIO qualified as UIO
import Web.Scotty.Trans (scottyT)

type Counter = TVar Integer

runRequestCounterIO :: (Reader Counter :> es, IOE :> es) => Eff (RequestCounter : es) a -> Eff es a
runRequestCounterIO = interpret $ const \case
  CurrentCount -> do
    counter <- Reader.ask
    UIO.atomically $ UIO.readTVar counter
  IncrementCount -> do
    counter <- Reader.ask
    UIO.atomically $ UIO.modifyTVar counter (+ (1 :: Integer))

runIO :: Counter -> Eff '[RequestCounter, Reader Counter, IOE] a -> IO a
runIO counter = runEff . Reader.runReader counter . runRequestCounterIO

main :: IO ()
main = do
  counter <- UIO.newTVarIO 0

  scottyT 3000 (runIO counter) Router.router
