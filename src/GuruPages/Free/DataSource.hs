module GuruPages.Free.DataSource
  ( getRawEntries,
    DataSource (..),
  )
where

import Free.AlaCarte
import GuruPages.Model
import Relude

type DataSource :: Type -> Type
data DataSource a
  = GetRawEntries GuruConfig ([RawEntry] -> a)
  deriving (Functor)

getRawEntries :: (DataSource :<: f) => GuruConfig -> Free f [RawEntry]
getRawEntries cfg = injectFree (GetRawEntries cfg Pure)
