{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GuruPages.Storage.Filesystem () where

import Data.Text
import Free.AlaCarte
import GuruPages.Free.Storage
import GuruPages.HtmlPage
import GuruPages.Model
import Optics
import Relude
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8

instance Exec Storage where
  {-# INLINEABLE execAlgebra #-}
  execAlgebra = \case
    RemovePath path f -> removePathForcibly path >> f
    CreatePath path f -> createDirectory path >> f
    WriteGuruPageToHtmlFile page f -> doWriteHtmlFile page >> f

doWriteHtmlFile :: (MonadIO m) => GuruPage -> m ()
doWriteHtmlFile page = do
  _ <- liftIO $ createDirectoryIfMissing True (fromString . unpack $ destinationFolder)
  writeFileBS (fromString . unpack $ page ^. #destinationPath) preparedFileContents
  where
    theFileName = pack . takeFileName . fromString . unpack $ page ^. #destinationPath
    destinationFolder = replace theFileName "" (page ^. #destinationPath)
    renderingResult = renderHtml $ wrapInNiceHtml page
    preparedFileContents = toStrict renderingResult
