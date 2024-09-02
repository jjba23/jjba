module GuruPages.Free.Storage
  ( removePath,
    createPath,
    writeGuruPageToHtmlFile,
    Storage (..),
  )
where

import Free.AlaCarte
import GuruPages.Model
import Relude

type Storage :: Type -> Type
data Storage a
  = RemovePath FilePath a
  | CreatePath FilePath a
  | WriteGuruPageToHtmlFile GuruPage a
  deriving (Functor)

removePath :: (Storage :<: f) => FilePath -> Free f ()
removePath path = injectFree (RemovePath path (Pure ()))

createPath :: (Storage :<: f) => FilePath -> Free f ()
createPath path = injectFree (CreatePath path (Pure ()))

writeGuruPageToHtmlFile :: (Storage :<: f) => GuruPage -> Free f ()
writeGuruPageToHtmlFile page = injectFree (WriteGuruPageToHtmlFile page (Pure ()))
