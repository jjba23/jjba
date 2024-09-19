{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module GuruPages.Model
  ( RawEntry (..),
    GuruPage (..),
    GuruConfig (..),
    guruConfigCodec,
  )
where

import Optics
import Relude
import Text.Pandoc
import Toml

type RawEntry :: Type
data RawEntry = RawEntry
  { htmlContents :: Text,
    pandoc :: Pandoc,
    destinationPath :: Text
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''RawEntry

type GuruPage :: Type
data GuruPage = GuruPage
  { htmlContents :: Text,
    destinationPath :: Text,
    authors :: [Text],
    pageDate :: Maybe Text,
    pageTitle :: Text
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''GuruPage

type GuruConfig :: Type
data GuruConfig = GuruConfig
  { destinationFolder :: Text,
    filePattern :: Text,
    readLocation :: Text,
    inputFormat :: Text,
    outputFormat :: Text
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''GuruConfig

guruConfigCodec :: TomlCodec GuruConfig
guruConfigCodec =
  GuruConfig
    <$> Toml.text "destination-folder"
    .= (^. #destinationFolder)
    <*> Toml.text "file-pattern"
    .= (^. #filePattern)
    <*> Toml.text "read-location"
    .= (^. #readLocation)
    <*> Toml.text "input-format"
    .= (^. #inputFormat)
    <*> Toml.text "output-format"
    .= (^. #outputFormat)
