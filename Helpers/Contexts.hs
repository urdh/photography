module Helpers.Contexts
  ( photoContext
  , collectionContext
  , archiveContext
  ) where

import           Control.Applicative            (Alternative (..))
import           Data.Binary                    (Binary (..))
import           Data.List                      (uncons)
import           Data.Time.Format               (defaultTimeLocale, formatTime)
import           Hakyll.Core.Compiler           (Compiler, getRoute, loadAll,
                                                 loadAllSnapshots)
import           Hakyll.Core.Identifier         (setVersion, toFilePath)
import           Hakyll.Core.Identifier.Pattern (Pattern, fromRegex, fromGlob,
                                                 hasNoVersion, (.&&.))
import           Hakyll.Core.Item               (Item (..))
import           Hakyll.Web.Html                (toUrl)
import           Hakyll.Web.Template.Context    (Context (..), defaultContext,
                                                 field, functionField,
                                                 listField, listFieldWith)
import           Helpers.Photos                 (alphabetical, chronological,
                                                 exifKeyField, getPhotoItemDate,
                                                 photoDateField, photoExifField,
                                                 photoFrameField,
                                                 photoRollField)
import           System.FilePath                (takeDirectory, (</>))
import           Type.Reflection                (Typeable)

photoContext :: Context String -> Pattern -> String -> Context String
photoContext rootContext indexPaths extensions =
  functionField "url"
    (\[v] -> fmap (maybe empty toUrl) . getRoute . setVersion (Just v) . itemIdentifier) <>
  listFieldWith   "parents" (collectionContext rootContext indexPaths extensions) collections <>
  photoFrameField "frame"           <>
  photoRollField  "roll"            <>
  photoDateField  "date" "%Y-%m-%d" <>
  photoExifField  "exif"            <>
  exifKeyField    "title" "title"   <>
  rootContext                       <>
  defaultContext
  where
    collectionItems = ((.&&.) indexPaths) . fromGlob . (\p -> takeDirectory p </> "*") . toFilePath
    collections = flip loadAllSnapshots "raw" . collectionItems . itemIdentifier

collectionContext :: Context String -> Pattern -> String -> Context String
collectionContext rootContext indexPaths extensions =
  field "updated"
    (fmap (maybe empty (photoDate "%Y-%m-%d" . fst) . uncons) . photos)          <>
  listFieldWith "photos" (photoContext rootContext indexPaths extensions) photos <>
  rootContext                                                                    <>
  defaultContext
  where
    collectionTiffs = fromRegex . (\p -> takeDirectory p </> extensions) . toFilePath
    photos = fmap (reverse . chronological) . (photosFrom . collectionTiffs . itemIdentifier)
    photoDate fmt = formatTime defaultTimeLocale fmt . getPhotoItemDate . itemIdentifier

archiveContext :: Context String -> Pattern -> Pattern -> String -> Context String
archiveContext rootContext indexPaths photoPaths extensions =
  listField "collections" (collectionContext rootContext indexPaths extensions) collections <>
  listField "photos"      (photoContext rootContext indexPaths extensions)      photos      <>
  rootContext                                                                               <>
  defaultContext
  where
    collections = alphabetical <$> loadAllSnapshots indexPaths "raw"
    photos = reverse . chronological <$> photosFrom photoPaths

--------------------------------------------------------------------------------
photosFrom :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
photosFrom p = loadAll (p .&&. hasNoVersion)
