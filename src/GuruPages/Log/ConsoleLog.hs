{-# OPTIONS_GHC -fno-warn-orphans #-}

module GuruPages.Log.ConsoleLog () where

import Data.Text
import Data.Time
import Free.AlaCarte
import GHC.Show
import GuruPages.Free.Log
import Relude hiding (Show, show)

instance Exec Logger where
  {-# INLINEABLE execAlgebra #-}
  execAlgebra (LogInfo msg next) = doLog Info msg next
  execAlgebra (LogError msg next) = doLog Error msg next
  execAlgebra (LogDebug msg next) = doLog Debug msg next

doLog :: (MonadIO m) => LogLevel -> Text -> m b -> m b
doLog level msg next = do
  now <- iso8601
  _ <- putStrLn (unpack $ logExpr now)
  next
  where
    logExpr now = now <> " " <> showLevel level <> " " <> msg

type LogLevel :: Type
data LogLevel = Info | Error | Debug deriving (Eq)

instance Show LogLevel where
  {-# INLINEABLE show #-}
  show Info = "[INFO]"
  show Error = "[ERROR]"
  show Debug = "[DEBUG]"

showLevel :: LogLevel -> Text
showLevel = pack . show

-- Construct format string according to <http://en.wikipedia.org/wiki/ISO_8601 ISO-8601>.
iso8601 :: (MonadIO m) => m Text
iso8601 = do
  n <- liftIO getCurrentTime
  let n' = pack $ formatTime defaultTimeLocale (unpack "%Y-%m-%dT%H:%M:%SZ") n
  pure $ "[" <> n' <> "]"
