{-# LANGUAGE TupleSections #-}

module Helpers.Contexts
  ( photoContext
  , collectionContext
  , archiveContext
  ) where

import           Control.Applicative            (Alternative (..))
import           Control.Conditional            (if')
import           Control.Lens.Lens              ((??))
import           Control.Monad                  (ap, join, msum, (<=<))
import           Data.Binary                    (Binary (..))
import           Data.List                      (singleton, sortOn)
import           Data.Maybe                     (fromMaybe)
import           Data.Time.Clock                (UTCTime)
import           Data.Time.Format               (TimeLocale, defaultTimeLocale, formatTime,
                                                 parseTimeM)
import           Hakyll.Core.Compiler           (Compiler, getRoute, loadAll,
                                                 loadAllSnapshots)
import           Hakyll.Core.Identifier         (Identifier, setVersion, toFilePath)
import           Hakyll.Core.Identifier.Pattern (Pattern, fromRegex, fromGlob,
                                                 hasNoVersion, (.&&.))
import           Hakyll.Core.Item               (Item (..))
import           Hakyll.Core.Metadata           (MonadMetadata (..), getMetadataField)
import           Hakyll.Web.Html                (toUrl)
import           Hakyll.Web.Template.Context    (Context (..), defaultContext,
                                                 field, functionField, getItemUTC,
                                                 listField, listFieldWith)
import           Helpers.Photos                 (chronological,
                                                 exifKeyField, getPhotoItemDate,
                                                 photoDateField, photoExifField,
                                                 photoFrameField,
                                                 photoRollField)
import           System.FilePath                (takeDirectory, takeBaseName, (</>))
import           Type.Reflection                (Typeable)

photoContext :: Context String -> Pattern -> String -> Context String
photoContext rootContext indexPaths extensions =
  functionField "url"
    (\v -> fmap (maybe empty toUrl) . getRoute . setVersion (Just . head $ v) . itemIdentifier) <>
  listFieldWith   "parents" (collectionContext rootContext indexPaths extensions) collections <>
  photoFrameField "frame"            <>
  photoRollField  "roll"             <>
  photoDateField  "taken" "%Y-%m-%d" <>
  photoExifField  "exif"             <>
  exifKeyField    "title" "title"    <>
  exifKeyField    "artist" "artist"  <>
  rootContext                        <>
  defaultContext
  where
    collectionItems = (.&&.) indexPaths . fromGlob . (\p -> takeDirectory p </> "*") . toFilePath
    collections = flip loadAllSnapshots "raw" . collectionItems . itemIdentifier

collectionContext :: Context String -> Pattern -> String -> Context String
collectionContext rootContext indexPaths extensions =
  field         "updated" (fmap formatDate . maximum' <=< getAllDates)            <>
  listFieldWith "photos"  (photoContext rootContext indexPaths extensions) photos <>
  rootContext                                                                     <>
  defaultContext
  where
    collectionTiffs = fromRegex . (\p -> takeDirectory p </> extensions) . toFilePath
    photos = fmap (reverse . chronological) . (photosFrom . collectionTiffs . itemIdentifier)
    formatDate = formatTime defaultTimeLocale "%Y-%m-%d"
    getPhotoDates = map (getPhotoItemDate . itemIdentifier)
    getUpdateDate = flip (<|>) (return []) . fmap singleton . getUpdateUTC defaultTimeLocale . itemIdentifier
    getPublishDate = flip (<|>) (return []) . fmap singleton . getItemUTC defaultTimeLocale  . itemIdentifier
    getAllDates = fmap join . sequence . ([getUpdateDate, getPublishDate, fmap getPhotoDates . photos] ??)
    maximum' = ap (flip if' empty . null) (return . maximum)

archiveContext :: Context String -> Pattern -> Pattern -> String -> Context String
archiveContext rootContext indexPaths photoPaths extensions =
  listField "collections" (collectionContext rootContext indexPaths extensions) collections <>
  listField "photos"      (photoContext rootContext indexPaths extensions)      photos      <>
  rootContext                                                                               <>
  defaultContext
  where
    collections = alphabetical =<< loadAllSnapshots indexPaths "raw"
    photos = reverse . chronological <$> photosFrom photoPaths

--------------------------------------------------------------------------------
photosFrom :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
photosFrom p = loadAll (p .&&. hasNoVersion)

alphabetical :: (MonadMetadata m) => [Item a] -> m [Item a]
alphabetical = sortByM $ ap ((<$>) . fromMaybe . takeBaseName . toFilePath)
                            (`getMetadataField` "sort-by") . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = map fst . sortOn snd <$> mapM (\x -> fmap (x,) (f x)) xs

getUpdateUTC :: (Alternative m, MonadMetadata m) => TimeLocale -> Identifier -> m UTCTime
getUpdateUTC locale id' = do
    field' <- getMetadataField id' "updated"
    maybe empty return $ msum [field' >>= parseTime' fmt | fmt <- formats]
  where
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%a, %d %b %Y %H:%M:%S"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M:%S"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S"
        , "%Y-%m-%d"
        , "%d.%m.%Y"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]
