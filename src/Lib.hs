{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-missed-specialisations #-}

module Lib
  ( runMain,
  )
where

import Data.Text hiding (map)
import Data.Time
import Free.AlaCarte
import GuruPages.Clock.LiveClock ()
import GuruPages.Configurator.FromFileConfigurator ()
import GuruPages.DataSource.OrgFilesDataSource ()
import GuruPages.Free.Clock
import GuruPages.Free.Configurator
import GuruPages.Free.DataSource
import GuruPages.Free.Log
import GuruPages.Free.Storage
import GuruPages.Log.ConsoleLog ()
import GuruPages.Model
import GuruPages.Storage.Filesystem ()
import Optics
import Relude
import Text.Pandoc

runMain :: IO ()
runMain = exec @(Configurator :+: Logger :+: Clock :+: Storage :+: DataSource) $ do
  programStartUTCTime <- now
  logInfo "Starting GuruPages!"
  logInfo "About to read configuration from file!"
  maybeCfg <- maybeReadConfig "guru-pages.toml"

  case maybeCfg of
    Left err -> do
      logInfo "No valid configuration could be read!"
      logInfo err
      logTimeElapsed programStartUTCTime
    Right cfg -> do
      logDebug (pack . show $ cfg)

      logInfo "About to find matching files!"
      rawEntries <- getRawEntries cfg

      let guruPages = map rawEntryToGuruPage rawEntries

      logInfo "About to remake the destination directory!"
      let destinationFolder = fromString . unpack $ cfg ^. #destinationFolder
      removePath destinationFolder >> createPath destinationFolder

      logInfo "About to write to HTML files!"
      mapM_ writeGuruPageToHtmlFile guruPages

      logInfo "Terminating hsResumeBuilder!"
      logTimeElapsed programStartUTCTime

logTimeElapsed ::
  (Clock :<: f, Logger :<: f) =>
  UTCTime ->
  Free f ()
logTimeElapsed programStartUTCTime = do
  t <- timeElapsedUntilNow programStartUTCTime
  logInfo ("Time elapsed: " <> t)

rawEntryToGuruPage :: RawEntry -> GuruPage
rawEntryToGuruPage entry = do
  GuruPage
    { htmlContents = entry ^. #htmlContents,
      destinationPath = entry ^. #destinationPath,
      authors = map prettyPrintInline (docAuthors meta),
      pageDate = if pageDateContent == "" then Nothing else Just pageDateContent,
      pageTitle = prettyPrintInline . docTitle $ meta
    }
  where
    (Pandoc meta _) = entry ^. #pandoc
    pageDateContent = prettyPrintInline . docDate $ meta

prettyPrintInline' :: Inline -> Text
prettyPrintInline' (Str t) = t
prettyPrintInline' Space = " "
prettyPrintInline' x = pack . show $ x

prettyPrintInline :: [Inline] -> Text
prettyPrintInline = Data.Text.intercalate "" . map prettyPrintInline'
