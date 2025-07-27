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
import qualified Data.ByteString             as DBS
import qualified Data.ByteString.Char8       as DBC
import           Data.Either                 (fromRight)
import           Data.List                   (sortOn)
import           Data.List.Extra             (headDef)
import qualified Data.Map.Strict             as DMS
import           Data.Maybe                  (catMaybes, fromJust)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, decodeASCII)
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
import           Text.XML.HXT.Core

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
    getValue "artist"   = unwords . map (prettify fixEncoding)             . findAll [artist]
    getValue "camera"   = unwords . map (prettify fixEncoding)             . findAll [make, model]
    getValue "lens"     = unwords . map (prettify fixEncoding)             . findAll [lensMake, lensModel]
    getValue "filmtype" = unwords . map (prettify id)                      . findAll [userComment]
    getValue "shutter"  = unwords . map (prettify (T.replace " sec." "s")) . findAll [exposureTime]
    getValue "aperture" = unwords . map (prettify (T.replace "f/" "ƒ/"))   . findAll [fnumber]
    getValue "speed"    = unwords . map (prettify id)                      . findAll [isoSpeedRatings]
    getValue "copyright"= unwords . map (prettify fixEncoding)             . findAll [copyright]
    getValue "license"  = headDef "" . concatMap (getXmlAttribute' "cc:license") . findAll [applNotes]
    getValue "location" = const "" -- TODO: read EXIF tag and do GPS lookup
    getValue "title"    = unwords . map (prettify fixEncoding)             . findAll [imageDescription]
    getValue tag        = error $ unwords ["Unknown EXIF field:", tag]
    -- Pretty-print an ExifValue given the ExifTag it belongs to
    prettify :: (T.Text -> T.Text) -> (ExifTag, ExifValue) -> String
    prettify f = T.unpack . f . uncurry prettyPrinter
    -- Fix encoding of string values by reinterpreting the string as UTF-8
    fixEncoding :: T.Text -> T.Text
    fixEncoding = decodeUtf8 . DBC.pack . T.unpack
    -- Interpret the given EXIF value as UTF-8 text
    asUtf8Text :: ExifValue -> Maybe T.Text
    asUtf8Text (ExifText       v) = Just . fixEncoding . T.pack $ v
    asUtf8Text (ExifNumberList v) = Just . decodeUtf8 . DBS.pack . map fromIntegral $ v
    asUtf8Text (ExifUndefined  v) = uncurry decodeExifUndefined . DBS.splitAt 8 $ v
    asUtf8Text _ = Nothing
    -- Interpret an ExifUndefined value given its prefix
    decodeExifUndefined :: DBC.ByteString -> DBC.ByteString -> Maybe T.Text
    decodeExifUndefined "ASCII\0\0\0" v = Just . decodeASCII $ v
    decodeExifUndefined "UNICODE\0"   v = Just . decodeUtf8 $ v
    decodeExifUndefined _ _ = Nothing
    -- Wrap getXmlAttribute to work with ExifValue items
    getXmlAttribute' :: String -> (ExifTag, ExifValue) -> [String]
    getXmlAttribute' key = maybe [] (getXmlAttribute key . T.unpack) . asUtf8Text . snd
    -- Get all values of the given attribute from the input XML
    getXmlAttribute :: String -> String -> [String]
    getXmlAttribute key = runLA (xreadDoc >>> multi (isElem >>> getAttrValue0 key))
    -- Keys not in hsexif
    applNotes = ExifTag IFD0 (Just "applicationNotes") 0x02bc $ T.pack . show
    lensMake  = ExifTag ExifSubIFD (Just "lensMake")  0xa433 $ T.pack . show
    lensModel = ExifTag ExifSubIFD (Just "lensModel") 0xa434 $ T.pack . show
