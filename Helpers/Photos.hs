{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Helpers.Photos
  ( getPhotoItemDate
  , photoDateField
  , photoRollField
  , photoFrameField
  , photoExifField
  , exifKeyField
  , chronological
  , alphabetical
  ) where

import           Control.Applicative         (Alternative (..))
import qualified Data.ByteString.Char8       as DBC
import           Data.Either                 (fromRight)
import           Data.List                   (sortOn)
import qualified Data.Map.Strict             as DMS
import           Data.Maybe                  (catMaybes, fromJust)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (UTCTime (..))
import           Data.Time.Format            (defaultTimeLocale, formatTime,
                                              parseTimeM)
import           Graphics.HsExif
import           Hakyll.Core.Identifier      (Identifier (..), toFilePath)
import           Hakyll.Core.Item            (Item (..))
import           Hakyll.Web.Template.Context (Context (..), field,
                                              functionField)
import           System.FilePath             (takeBaseName)
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.RE.TDFA.String

getPhotoItemDate :: Identifier -> UTCTime
getPhotoItemDate id' =
  fromJust $
  parseTime' . fst3 . splitPhotoFilename . takeBaseName . toFilePath $ id'
  where
    parseTime' = parseTimeM False defaultTimeLocale "%Y%m%d"
    fst3 (a, _, _) = a

photoDateField :: String -> String -> Context a
photoDateField key format =
  field key $ \i ->
    return $
    formatTime defaultTimeLocale format . getPhotoItemDate $ itemIdentifier i

photoRollField :: String -> Context a
photoRollField key =
  field key $ \i ->
    return $
    snd3 . splitPhotoFilename . takeBaseName . toFilePath $ itemIdentifier i
  where
    snd3 (_, b, _) = b

photoFrameField :: String -> Context a
photoFrameField key =
  field key $ \i ->
    return $
    trd3 . splitPhotoFilename . takeBaseName . toFilePath $ itemIdentifier i
  where
    trd3 (_, _, c) = c

photoExifField :: String -> Context a
photoExifField key =
  functionField key $ \[k] i ->
    (\v ->
       if null v
         then empty
         else return v) $
    getExifValue k $ getExifMap i

exifKeyField :: String -> String -> Context a
exifKeyField key k =
  field key $ \i ->
    (\v ->
       if null v
         then empty
         else return v) $
    getExifValue k $ getExifMap i

chronological :: [Item a] -> [Item a]
chronological = sortOn $ getPhotoItemDate . itemIdentifier

alphabetical :: [Item a] -> [Item a]
alphabetical = sortOn $ takeBaseName . toFilePath . itemIdentifier

splitPhotoFilename :: String -> (String, String, String)
splitPhotoFilename str =
  matches' $ str =~ [re|([0-9]{8})-((A|D)[0-9]{4})\+([0-9]{3})|]
  where
    matches' :: [[String]] -> (String, String, String)
    matches' [_:date:roll:_:frame:_] = (date, roll, dropWhile (== '0') frame)
    matches' xs = error $ unwords ["Invalid photo filename:", show xs]

--------------------------------------------------------------------------------
getExifMap :: Item a -> DMS.Map ExifTag ExifValue
getExifMap = fromRight DMS.empty . unsafePerformIO . parseFileExif . toFilePath . itemIdentifier

getExifValue :: String -> DMS.Map ExifTag ExifValue -> String
getExifValue = getValue
  where
    -- Finding EXIF values in the EXIF key-value map
    findAll :: [ExifTag] -> DMS.Map ExifTag ExifValue -> [(ExifTag, ExifValue)]
    findAll = (catMaybes .) . flip (map . flip findOne)
    findOne :: ExifTag -> DMS.Map ExifTag ExifValue -> Maybe (ExifTag, ExifValue)
    findOne key' = withKey . DMS.lookup key'
      where
        withKey (Just value) = Just (key', value)
        withKey _            = Nothing
    -- Maps string keys to specific (potentially composed) EXIF tags
    -- See http://hackage.haskell.org/package/hsexif-0.6.1.6/docs/Graphics-HsExif.html for keys
    getValue :: String -> DMS.Map ExifTag ExifValue -> String
    getValue "camera"   = unwords . map (prettify fixEncoding)             . findAll [make, model]
    getValue "lens"     = unwords . map (prettify fixEncoding)             . findAll [lensMake, lensModel]
    getValue "filmtype" = unwords . map (prettify id)                      . findAll [userComment]
    getValue "shutter"  = unwords . map (prettify (T.replace " sec." "s")) . findAll [exposureTime]
    getValue "aperture" = unwords . map (prettify (T.replace "f/" "ƒ/"))   . findAll [fnumber]
    getValue "speed"    = unwords . map (prettify id)                      . findAll [isoSpeedRatings]
    getValue "copyright"= unwords . map (prettify fixEncoding)             . findAll [copyright]
    getValue "license"  = const "" -- TODO: read XMP tag cc:license
    getValue "location" = const "" -- TODO: read EXIF tag and do GPS lookup
    getValue "title"    = unwords . map (prettify fixEncoding)             . findAll [imageDescription]
    getValue tag        = error $ unwords ["Unknown EXIF field:", tag]
    -- Pretty-print an ExifValue given the ExifTag it belongs to
    prettify :: (T.Text -> T.Text) -> (ExifTag, ExifValue) -> String
    prettify f = T.unpack . f . uncurry prettyPrinter
    -- Fix encoding of string values by reinterpreting the string as UTF-8
    fixEncoding :: T.Text -> T.Text
    fixEncoding = decodeUtf8 . DBC.pack . T.unpack
    -- Keys not in hsexif
    lensMake  = ExifTag ExifSubIFD (Just "lensMake")  0xa433 $ T.pack . show
    lensModel = ExifTag ExifSubIFD (Just "lensModel") 0xa434 $ T.pack . show
