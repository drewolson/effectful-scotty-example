module Fun.Router
  ( router,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Effectful (Eff, IOE, type (:>))
import Fun.RequestCounter (RequestCounter (..))
import Fun.RequestCounter qualified as RequestCounter
import Web.Scotty.Trans (ScottyT, get, html)

router :: (RequestCounter :> es, IOE :> es) => ScottyT Text (Eff es) ()
router = do
  get "/" do
    lift RequestCounter.incrementCount
    count <- lift RequestCounter.currentCount

    html $ "Current count: " <> Text.pack (show count)
