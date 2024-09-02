{-# OPTIONS_GHC -fno-warn-orphans #-}

module GuruPages.Clock.LiveClock () where

import Data.Text (pack)
import Data.Time
import Free.AlaCarte
import GuruPages.Free.Clock
import Relude

instance Exec Clock where
  {-# INLINEABLE execAlgebra #-}
  execAlgebra (TimeElapsedUntilNow fromTime f) = do
    diffWithNow fromTime >>= f
  execAlgebra (Now f) = do
    getCurrentTime >>= f

diffWithNow :: (MonadIO m) => UTCTime -> m Text
diffWithNow fromTime = do
  now' <- liftIO getCurrentTime
  pure $ pack . show $ diffUTCTime now' fromTime
