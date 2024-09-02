module GuruPages.Free.Configurator
  ( maybeReadConfig,
    Configurator (..),
  )
where

import Free.AlaCarte
import GuruPages.Model (GuruConfig)
import Relude

type Configurator :: Type -> Type
data Configurator a
  = ReadConfig FilePath (Either Text GuruConfig -> a)
  deriving (Functor)

maybeReadConfig :: (Configurator :<: f) => FilePath -> Free f (Either Text GuruConfig)
maybeReadConfig filePath = injectFree (ReadConfig filePath Pure)
