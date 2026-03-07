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
  ) where

import           Control.Applicative         (Alternative (..), (<|>), asum)
import           Control.Monad               (ap)
import qualified Data.ByteString.Char8       as DBC
import           Data.List                   (intercalate, sortOn)
import qualified Data.Map.Strict             as DMS
import           Data.Maybe                  (fromJust, listToMaybe)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Text.Normalize
import           Data.Time.Clock             (UTCTime (..))
import           Data.Time.Format            (defaultTimeLocale, formatTime, parseTimeM)
import           Graphics.HsExif
import           Hakyll.Core.Identifier      (Identifier (..), toFilePath)
import           Hakyll.Core.Item            (Item (..))
import           Hakyll.Web.Template.Context (Context (..), field, functionField)
import           Helpers.Metadata            (Metadata (..), loadMetadata)
import           System.FilePath             (takeBaseName)
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
  functionField key $ \k (Item i _) ->
    loadMetadata i >>= maybe empty return . (getMetadataValue . head $ k)

exifKeyField :: String -> String -> Context a
exifKeyField key k =
  field key $ \(Item i _) -> do
    loadMetadata i >>= maybe empty return . getMetadataValue k

chronological :: [Item a] -> [Item a]
chronological = sortOn $ getPhotoItemDate . itemIdentifier

splitPhotoFilename :: String -> (String, String, String)
splitPhotoFilename str =
  matches' $ str =~ [re|([0-9]{8})-((A|D)[0-9]{4})\+([0-9]{3})|]
  where
    matches' :: [[String]] -> (String, String, String)
    matches' [_:date:roll:_:frame:_] = (date, roll, dropWhile (== '0') frame)
    matches' xs = error $ unwords ["Invalid photo filename:", show xs]

--------------------------------------------------------------------------------
getMetadataValue :: String -> [Metadata] -> Maybe String
getMetadataValue = (asum .) . map . getValue
  where
    getValue :: String -> Metadata -> Maybe String
    getValue k (ExifData e) = getExifValue k e
    getValue k (XmpData e) = getXmpValue k e

getXmpValue :: String -> T.Text -> Maybe String
getXmpValue = (. T.unpack) . getValue
  where
    getValue :: String -> (String -> Maybe String)
    getValue "artist"    = listToMaybe . findXmpValue "dc:creator"
    getValue "copyright" = listToMaybe . findXmpValue "dc:rights"
    getValue "license"   = listToMaybe . findXmpValue "cc:license"
    getValue "title"     = listToMaybe . findXmpValue "dc:description"
    getValue "location"  = fmap (intercalate ", ") <$>
                            findXmpValues ["photoshop:City", "photoshop:Country"]
    getValue _           = const Nothing
    -- Get all values of the given attribute from the input XML (TODO: too lenient!)
    findXmpValues :: [String] -> String -> Maybe [String]
    findXmpValues = flip (mapM . (listToMaybe .) . flip findXmpValue)
    findXmpValue :: String -> String -> [String]
    findXmpValue key = runLA (xreadDoc >>> multi (
                           (isElem >>> getAttrValue0 key) <+>
                           (isElem >>> hasName key /> isText
                             `notContaining` isWhiteSpace >>> getText) <+>
                           (isElem >>> hasName key >>> getAttrValue0 "rdf:resource") <+>
                           (isElem >>> hasName key /> (hasName "rdf:Seq" <+> hasName "rdf:Alt")
                             /> hasName "rdf:li" /> isText >>> getText)
                       ))

getExifValue :: String -> DMS.Map ExifTag ExifValue -> Maybe String
getExifValue = getValue
  where
    -- Finding EXIF values in the EXIF key-value map
    findKeys :: [ExifTag] -> DMS.Map ExifTag ExifValue -> Maybe [(ExifTag, ExifValue)]
    findKeys = (sequence .) . flip (map . flip findKey)
    findKey :: ExifTag -> DMS.Map ExifTag ExifValue -> Maybe (ExifTag, ExifValue)
    findKey key' = withKey . DMS.lookup key'
      where
        withKey (Just value) = Just (key', value)
        withKey _            = Nothing
    -- Maps string keys to specific (potentially composed) EXIF tags
    -- See http://hackage.haskell.org/package/hsexif-0.6.1.6/docs/Graphics-HsExif.html for keys
    getValue :: String -> (DMS.Map ExifTag ExifValue -> Maybe String)
    getValue "artist"    = fmap (prettify fixEncoding) . findKey artist
    getValue "camera"    = ((asum .) . flip (map . flip id)) $
                            map (fmap (unwords . map (prettify fixEncoding)) .)
                            [findKeys [make, model], findKeys [cameraMake, cameraModel]]
    getValue "lens"      = (fmap (unwords . map (prettify fixEncoding)) .) =<<
                            (. findKeys [lensMake, lensModel]) . fmap . unclutter .
                            ap ((<|>) . findKey make) (findKey cameraMake)
    getValue "filmtype"  = fmap (prettify id) . findKey userComment
    getValue "shutter"   = ((asum .) . flip (map . flip id)) $
                            map (fmap (prettify (T.replace " sec." "s")) .)
                            [findKey exposureTime, findKey shutterSpeedValue]
    getValue "aperture"  = ((asum .) . flip (map . flip id)) $
                            map (fmap (prettify (T.replace "f/" "ƒ/")) .)
                            [findKey fnumber, findKey apertureValue]
    getValue "speed"     = fmap (prettify id) . findKey isoSpeedRatings
    getValue "copyright" = fmap (prettify fixEncoding) . findKey copyright
    getValue "title"     = fmap (prettify fixEncoding) . findKey imageDescription
    getValue _           = const Nothing
    -- Pretty-print an ExifValue given the ExifTag it belongs to
    prettify :: (T.Text -> T.Text) -> (ExifTag, ExifValue) -> String
    prettify f = T.unpack . f . uncurry prettyPrinter
    -- Fix encoding of string values by reinterpreting the string as UTF-8
    fixEncoding :: T.Text -> T.Text
    fixEncoding = normalize NFC . decodeUtf8 . DBC.pack . T.unpack
    -- Unclutter the lens make by dropping is if it is the same as the camera make
    unclutter :: Maybe (ExifTag, ExifValue) -> [(ExifTag, ExifValue)] -> [(ExifTag, ExifValue)]
    unclutter (Just (_, x)) (y:xs) = [y | x /= snd y] ++ xs
    unclutter _ xs = xs
    -- Keys not in hsexif
    lensMake    = ExifTag ExifSubIFD (Just "lensMake")    0xa433 $ T.pack . show
    lensModel   = ExifTag ExifSubIFD (Just "lensModel")   0xa434 $ T.pack . show
    cameraMake  = ExifTag ExifSubIFD (Just "cameraMake")  0x010f $ T.pack . show
    cameraModel = ExifTag ExifSubIFD (Just "cameraModel") 0x0110 $ T.pack . show
