{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GuruPages.DataSource.OrgFilesDataSource () where

import Data.Text
import Free.AlaCarte
import GuruPages.Free.DataSource
import GuruPages.Model
import Optics
import Relude
import System.FilePath.Glob
import Text.Pandoc
import Text.Pandoc.Highlighting

instance Exec DataSource where
  {-# INLINEABLE execAlgebra #-}
  execAlgebra (GetRawEntries cfg next) = do
    orgFilePaths <- liftIO $ globDir1 (compile (unpack $ cfg ^. #filePattern)) (unpack $ "./" <> cfg ^. #readLocation)
    rawEntries <- mapM (orgFileToEntry cfg) orgFilePaths
    next rawEntries

orgTextToHtml :: (MonadIO m) => Text -> m (Text, Pandoc)
orgTextToHtml txt = liftIO . runIOorExplode $ do
  pandoc <- readOrg readerOptions txt
  htmlContents <- writeHtml5String writerOptions pandoc
  pure (htmlContents, pandoc)
  where
    writerOptions = def {writerReferenceLinks = True, writerHighlightStyle = Just haddock, writerTabStop = 4}
    readerOptions = def {readerTabStop = 4}

orgFileToEntry :: (MonadIO m) => GuruConfig -> FilePath -> m RawEntry
orgFileToEntry cfg orgFilePath = do
  fileContents <- liftIO $ decodeUtf8 <$> readFileBS orgFilePath
  (htmlContents, pandoc) <- orgTextToHtml fileContents
  pure
    $ RawEntry
      { htmlContents = htmlContents,
        pandoc = pandoc,
        destinationPath = makeDestinationFilePath . pack $ orgFilePath
      }
  where
    makeDestinationFilePath =
      replace (cfg ^. #readLocation) ""
        . replace "./" (cfg ^. #destinationFolder)
        . replace (cfg ^. #inputFormat) (cfg ^. #outputFormat)
