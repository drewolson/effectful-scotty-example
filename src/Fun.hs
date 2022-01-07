module Fun
  ( RequestCounter (..),
    main,
    router,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Effectful
  ( Dispatch (..),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    interpret,
    runEff,
    runPureEff,
    send,
    type (:>),
  )
import Effectful.Reader (Reader)
import Effectful.Reader qualified as Reader
import UnliftIO (TVar)
import UnliftIO qualified as UIO
import Web.Scotty.Trans (ScottyError, ScottyT, get, html, scottyT)

data RequestCounter :: Effect where
  CurrentCount :: RequestCounter m Integer
  IncrementCount :: RequestCounter m ()

type instance DispatchOf RequestCounter = 'Dynamic

currentCount :: (RequestCounter :> es) => Eff es Integer
currentCount = send CurrentCount

incrementCount :: (RequestCounter :> es) => Eff es ()
incrementCount = send IncrementCount

runRequestCounterIO :: (Reader (TVar Integer) :> es, IOE :> es) => Eff (RequestCounter : es) a -> Eff es a
runRequestCounterIO = interpret $ const \case
  CurrentCount -> do
    count <- Reader.ask
    UIO.atomically $ UIO.readTVar count
  IncrementCount -> do
    count <- Reader.ask
    UIO.atomically $ UIO.modifyTVar count (+ 1)

runIO :: TVar Integer -> Eff '[RequestCounter, Reader (TVar Integer), IOE] a -> IO a
runIO count = runEff . Reader.runReader count . runRequestCounterIO

router :: (MonadIO (Eff es), RequestCounter :> es) => ScottyT Text (Eff es) ()
router = do
  get "/" do
    lift incrementCount
    count <- lift currentCount

    html $ "Current count: " <> Text.pack (show count)

main :: IO ()
main = do
  count <- UIO.newTVarIO 0

  scottyT 3000 (runIO count) router
