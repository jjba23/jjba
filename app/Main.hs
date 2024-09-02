module Main (main) where

import Lib
import Relude

main :: (MonadIO m) => m ()
main = liftIO runMain
