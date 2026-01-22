{-# LANGUAGE OverloadedStrings #-}

module Helpers.Metadata
  ( Metadata (..)
  , readJpegMetadata
  , readTiffMetadata
  , readMetadata
  ) where

import           Control.Monad               ((<=<), unless)
import           Data.Binary.Get             (Get, getByteString, getWord16be, getWord8, runGet)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Char8       as BC
import           Data.Either.Extra           (eitherToMaybe)
import qualified Data.Map.Strict             as DMS
import           Data.Maybe                  (maybeToList)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, decodeASCII)
import           Data.Text.Normalize
import           Data.Word                   (Word8)
import           Graphics.HsExif

--------------------------------------------------------------------------------
data Metadata =
    ExifData (DMS.Map ExifTag ExifValue)
  | XmpData  !T.Text
  deriving (Eq, Show)

readJpegMetadata :: L.ByteString -> [Metadata]
readJpegMetadata = concatMap getExifXmp . runGet getJpegMetadata

readTiffMetadata :: L.ByteString -> [Metadata]
readTiffMetadata = concatMap getExifXmp . maybeToList . fmap ExifData . eitherToMaybe . parseExif

readMetadata :: L.ByteString -> [Metadata] -- TODO: Sometimes causes "not enough bytes" on TIFF files?
readMetadata str
  | tiffle `L.isPrefixOf` str = readTiffMetadata str
  | tiffbe `L.isPrefixOf` str = readTiffMetadata str
  | jpeg   `L.isPrefixOf` str = readJpegMetadata str
  | otherwise = []
  where
    tiffle = "II\x2A\x00"
    tiffbe = "MM\x00\x2A"
    jpeg = "\xFF\xD8"

--------------------------------------------------------------------------------
getExifXmp :: Metadata -> [Metadata]
getExifXmp (XmpData xmp)   = [XmpData xmp]
getExifXmp (ExifData exif) = reverse $ (maybeToList . getXmpData $ exif) ++
                                       [ExifData . DMS.delete applNotes $ exif]
  where
    -- Get the XMP data from the application notes EXIF value
    getXmpData :: DMS.Map ExifTag ExifValue -> Maybe Metadata
    getXmpData = fmap XmpData . (asUtf8Text <=< DMS.lookup applNotes)
    -- Interpret the given EXIF value as UTF-8 text
    asUtf8Text :: ExifValue -> Maybe T.Text
    asUtf8Text (ExifText       v) = Just . normalize NFC . decodeUtf8 . BC.pack $ v
    asUtf8Text (ExifNumberList v) = Just . decodeUtf8 . B.pack . map fromIntegral $ v
    asUtf8Text (ExifUndefined  v) = uncurry decodeExifUndefined . B.splitAt 8 $ v
    asUtf8Text _ = Nothing
    -- Interpret an ExifUndefined value given its prefix
    decodeExifUndefined :: BC.ByteString -> BC.ByteString -> Maybe T.Text
    decodeExifUndefined "ASCII\0\0\0" v = Just . decodeASCII $ v
    decodeExifUndefined "UNICODE\0"   v = Just . decodeUtf8 $ v
    decodeExifUndefined _ _ = Nothing
    -- Keys not in hsexif
    applNotes = ExifTag IFD0 (Just "applicationNotes") 0x02bc $ T.pack . show

--------------------------------------------------------------------------------
data JpegFrameKind =
    JpegStartOfImage
  | JpegEndOfImage
  | JpegStartOfScan
  | JpegAppSegment Word8
  | JpegExtSegment Word8
  | IgnoredSegment Word8
  deriving (Eq, Show)

getJpegMetadata :: Get [Metadata]
getJpegMetadata = do
  marker <- getNextMarker
  kind <- case marker of
    0xD8 -> return JpegStartOfImage
    0xD9 -> return JpegEndOfImage
    0xDA -> return JpegStartOfScan
    a | a >= 0xF0 -> return $! JpegExtSegment a
      | a >= 0xE0 -> return $! JpegAppSegment (a - 0xE0)
      | otherwise   -> return $! IgnoredSegment a
  case kind of
    JpegEndOfImage -> pure []
    _ -> do
      meta <- parseJpegSegment kind
      remaining <- getJpegMetadata
      return $ maybeToList meta ++ remaining

getNextMarker :: Get Word8
getNextMarker = do
  skipUntilMarker
  marker <- getWord8
  case marker of
    a | a == 0x00              -> getNextMarker -- nul
      | a >= 0xD0 && a <= 0xD7 -> getNextMarker -- restart marker
      | otherwise              -> return a      -- actual marker

skipUntilMarker :: Get ()
skipUntilMarker = do
  byte <- getWord8
  unless (byte == 0xFF) skipUntilMarker

parseJpegSegment :: JpegFrameKind -> Get (Maybe Metadata)
parseJpegSegment JpegStartOfScan    = Nothing <$ skipUntilMarker
parseJpegSegment (JpegAppSegment 1) = parseAppSegment <$> takeCurrentFrame
parseJpegSegment JpegStartOfImage   = return Nothing
parseJpegSegment _                  = return Nothing

takeCurrentFrame :: Get B.ByteString
takeCurrentFrame = do
  size <- getWord16be
  getByteString (fromIntegral size - 2)

parseAppSegment :: B.ByteString -> Maybe Metadata
parseAppSegment str
  | exifHeader `B.isPrefixOf` str = fmap ExifData . eitherToMaybe . parseExif . L.fromStrict .
                                    B.drop (B.length exifHeader) $ str
  | xmpHeader  `B.isPrefixOf` str = fmap XmpData . Just . normalize NFC . decodeUtf8 .
                                    B.drop (B.length xmpHeader) $ str
  | otherwise = Nothing
  where
    exifHeader = BC.pack "Exif\0\0"
    xmpHeader = BC.pack "http://ns.adobe.com/xap/1.0/\0"
