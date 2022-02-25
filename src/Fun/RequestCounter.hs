module Fun.RequestCounter
  ( RequestCounter (..),
    currentCount,
    incrementCount,
  )
where

import Effectful (Effect)
import Effectful.TH (makeEffect)

data RequestCounter :: Effect where
  CurrentCount :: RequestCounter m Integer
  IncrementCount :: RequestCounter m ()

makeEffect ''RequestCounter
