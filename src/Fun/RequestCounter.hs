module Fun.RequestCounter
  ( RequestCounter (..),
    currentCount,
    incrementCount,
  )
where

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, type (:>))
import Effectful.Dispatch.Dynamic (send)

data RequestCounter :: Effect where
  CurrentCount :: RequestCounter m Integer
  IncrementCount :: RequestCounter m ()

type instance DispatchOf RequestCounter = 'Dynamic

currentCount :: (RequestCounter :> es) => Eff es Integer
currentCount = send CurrentCount

incrementCount :: (RequestCounter :> es) => Eff es ()
incrementCount = send IncrementCount
