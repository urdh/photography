{-# LANGUAGE OverloadedStrings #-}

module Helpers.Metadata
  ( Metadata (..)
  , loadMetadata
  , saveMetadata
  ) where

import           Control.Arrow               ((***))
import           Control.Monad               ((<=<), unless, liftM, void)
import           Data.Binary                 (Binary (..))
import           Data.Binary.Get             (Get, getByteString, getWord16be, getWord8, runGet)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Char8       as BC
import           Data.Coerce                 (coerce)
import           Data.Either.Extra           (eitherToMaybe)
import qualified Data.Map.Strict             as DMS
import           Data.Maybe                  (maybeToList)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, decodeASCII)
import           Data.Text.Normalize
import           Data.Tuple.Extra            (uncurry3)
import           Data.Word                   (Word8, Word16)
import           Graphics.HsExif
import           Hakyll.Core.Compiler        (Compiler, Snapshot, cached, getResourceLBS,
                                              loadSnapshotBody, saveSnapshot)
import           Hakyll.Core.Identifier      (Identifier (..))

--------------------------------------------------------------------------------
data Metadata =
    ExifData (DMS.Map ExifTag ExifValue)
  | XmpData  !T.Text
  deriving (Eq, Show)

loadMetadata ::  Identifier -> Compiler [Metadata]
loadMetadata = flip loadSnapshotBody metadataSnapshot

saveMetadata :: Compiler ()
saveMetadata = void
  $ cached "Helpers.Metadata.metadataCompiler" (fmap readMetadata <$> getResourceLBS)
    >>= saveSnapshot metadataSnapshot

--------------------------------------------------------------------------------
instance Binary Metadata where
  put (ExifData exif) = do put (0 :: Word8)
                           put . map (ExifTag_ *** ExifValue_) . DMS.toList $ exif
  put (XmpData xmp)   = do put (1 :: Word8)
                           put xmp
  get = do tag <- getWord8
           case tag of
             0 -> liftM (ExifData . mapFromList) get
             1 -> liftM XmpData get
             _ -> error "unhandled metadata marker"

mapFromList :: [(ExifTag_, ExifValue_)] -> DMS.Map ExifTag ExifValue
mapFromList = DMS.fromList . map (coerce *** coerce)

newtype ExifTag_ = ExifTag_ ExifTag

instance Binary ExifTag_ where
  put (ExifTag_ (ExifTag loc _ key _)) = do put key
                                            put $ case loc of
                                                    ExifSubIFD -> (0 :: Word8)
                                                    GpsSubIFD -> (1 :: Word8)
                                                    IFD0 -> (2 :: Word8)
  get = do key <- get :: Get Word16
           loc <- get :: Get Word8
           case loc of
             0 -> return $ ExifTag_ (ExifTag ExifSubIFD Nothing key (T.pack . show))
             1 -> return $ ExifTag_ (ExifTag GpsSubIFD Nothing key (T.pack . show))
             2 -> return $ ExifTag_ (ExifTag IFD0 Nothing key (T.pack . show))
             _ -> error "unhandled exif tag marker"

newtype ExifValue_ = ExifValue_ ExifValue

instance Binary ExifValue_ where
  put (ExifValue_ (ExifNumber n))        = do put (0 :: Word8)
                                              put n
  put (ExifValue_ (ExifText t))          = do put (1 :: Word8)
                                              put t
  put (ExifValue_ (ExifRational a b))    = do put (2 :: Word8)
                                              put (a, b)
  put (ExifValue_ (ExifNumberList l))    = do put (3 :: Word8)
                                              put l
  put (ExifValue_ (ExifRationalList l))  = do put (4 :: Word8)
                                              put l
  put (ExifValue_ (ExifUndefined b))     = do put (5 :: Word8)
                                              put b
  put (ExifValue_ (ExifUnknown t c v))   = do put (6 :: Word8)
                                              put (t, c, v)
  get = do tag <- getWord8
           case tag of
             0 -> fmap (ExifValue_ . ExifNumber) get
             1 -> fmap (ExifValue_ . ExifText) get
             2 -> fmap (ExifValue_ . uncurry ExifRational) get
             3 -> fmap (ExifValue_ . ExifNumberList) get
             4 -> fmap (ExifValue_ . ExifRationalList) get
             5 -> fmap (ExifValue_ . ExifUndefined) get
             6 -> fmap (ExifValue_ . uncurry3 ExifUnknown) get
             _ -> error "unhandled exif value marker"

--------------------------------------------------------------------------------
metadataSnapshot :: Snapshot
metadataSnapshot = "_Helpers.Metadata"

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
