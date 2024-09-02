{-# OPTIONS_GHC -fno-warn-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GuruPages.Configurator.FromFileConfigurator () where

import Free.AlaCarte
import GuruPages.Free.Configurator
import GuruPages.Model
import Relude
import Toml hiding (first)

instance Exec Configurator where
  {-# INLINEABLE execAlgebra #-}
  execAlgebra (ReadConfig filePath next) = do
    errOrConfig <- decodeFileEither guruConfigCodec filePath
    next $ first prettyTomlDecodeErrors errOrConfig
