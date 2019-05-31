--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Control.Applicative          (Alternative(..))
import           Control.Monad                (liftM, ap)
import           System.FilePath              ((</>), takeDirectory, takeBaseName)
import           System.IO.Unsafe             (unsafePerformIO)
import           Data.Time.Clock              (UTCTime (..))
import           Data.Time.Format             (TimeLocale, parseTimeM, defaultTimeLocale, formatTime)
import           Data.Ord                     (comparing)
import           Data.List                    (intercalate, sortBy, sortOn, uncons, isSuffixOf)
import           Data.Binary                  (Binary)
import           Data.Typeable                (Typeable)
import qualified Data.Map.Strict       as DMS (empty, lookup)
import           Data.Map.Strict              ((!), (!?), Map(..))
import           Data.Either                  (fromRight)
import qualified Data.Text             as T   (pack, unpack, replace, append, Text(..))
import           Data.Text.Encoding           (decodeUtf8, decodeLatin1, encodeUtf8)
import           Data.Maybe                   (catMaybes, fromJust)
import           Data.ByteString.Char8 as DBC (pack)
import           Hakyll                hiding (chronological)
import           Hakyll.Images                (loadImage, ensureFitCompiler, rotateFromExifCompiler)
import           Text.Regex.Base.Context
import           Text.RE.TDFA.String
import           Graphics.HsExif


--------------------------------------------------------------------------------
root :: String
root = "http://localhost:8000"

config :: Configuration
config = (defaultConfiguration)

photoPathRegex :: Pattern
photoPathRegex = fromRegex "collections/.+/.+\\.(tif|tiff|jpg|jpeg)"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config  $ do
    -- Collections
    match "collections/*/index.*" $ do
        route   $ setExtension "html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "raw"
                >>= loadAndApplyTemplate "templates/collection.html" collectionContext
                >>= loadAndApplyTemplate "templates/default.html"    defaultContext'
                >>= cleanIndexUrls

    -- Photographies in a collection
    match photoPathRegex $
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/photo.html" photoContext
                >>= cleanIndexUrls
    match photoPathRegex $ version "480px"  $ resize "jpeg" 480
    match photoPathRegex $ version "800px"  $ resize "jpeg" 800
    match photoPathRegex $ version "960px"  $ resize "jpeg" 960

    -- Style sheets and assets
    match "*.css" $ do
        route   $ idRoute
        compile $ compressCssCompiler

    -- Index
    match "index.html" $ do
        route   $ idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate                               archiveContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext'
                >>= cleanIndexUrls

    -- Recent photos JSON
    create ["photos.json"] $ do
        route   $ idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/photos.json" archiveContext
                >>= cleanIndexUrls

    -- Sitemap
    create ["sitemap.xml"] $ do
        route   $ idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" archiveContext
                >>= cleanIndexUrls

    -- Templates
    match "templates/sidebar.html" $
        compile $ do
            getResourceBody
                >>= applyAsTemplate archiveContext
                >>= cleanIndexUrls
    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
resize :: String -> Int -> Rules()
resize ext h = do
    route   $ setExtension $ (show h) ++ "px." ++ ext
    compile $ loadImage >>= ensureFitCompiler 65535 h >>= rotateFromExifCompiler


--------------------------------------------------------------------------------
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls (foldr (.) id (map stripSuffix index')))
    where
        index' = ["index.html", "index.htm"]

stripSuffix :: String -> String -> String
stripSuffix suffix' str
    | suffix' `isSuffixOf` str = take (length str - length suffix') str
    | otherwise                = str


--------------------------------------------------------------------------------
splitPhotoFilename :: String -> (String, String, String)
splitPhotoFilename str =
    matches $ str =~ [re|([0-9]{8})-((A|D)[0-9]{4})\+([0-9]{3})|]
    where
        matches :: [[String]] -> (String, String, String)
        matches [(tmp:date:roll:_:frame:_)] = (date, roll, dropWhile (=='0') $ frame)

getPhotoItemDate :: Identifier -> UTCTime
getPhotoItemDate id' =
    fromJust $ parseTime' . fst3 . splitPhotoFilename . takeBaseName . toFilePath $ id'
    where
        parseTime' = parseTimeM False defaultTimeLocale "%Y%m%d"
        fst3 (a, b, c) = a


--------------------------------------------------------------------------------
photoDateField :: String -> String -> Context a
photoDateField key format = field key $ \i -> do
    return $ formatTime defaultTimeLocale format . getPhotoItemDate $ itemIdentifier i

photoRollField :: String -> Context a
photoRollField key = field key $ \i -> do
    return $ snd3 . splitPhotoFilename . takeBaseName . toFilePath $ itemIdentifier i
    where
        snd3 (a, b, c) = b

photoFrameField :: String -> Context a
photoFrameField key = field key $ \i -> do
    return $ trd3 . splitPhotoFilename . takeBaseName . toFilePath $ itemIdentifier i
    where
        trd3 (a, b, c) = c


--------------------------------------------------------------------------------
getExifValue :: String -> (Map ExifTag ExifValue) -> String
getExifValue = getValue
    where
        -- Finding EXIF values in the EXIF key-value map
        findAll :: [ExifTag] -> (Map ExifTag ExifValue) -> [(ExifTag, ExifValue)]
        findAll = (catMaybes .) . flip (map . flip findOne)
        findOne :: ExifTag -> (Map ExifTag ExifValue) -> Maybe (ExifTag, ExifValue)
        findOne key' = withKey . DMS.lookup key'
            where
                withKey (Just value) = Just (key', value)
                withKey _            = Nothing
        -- Maps string keys to specific (potentially composed) EXIF tags
        -- See http://hackage.haskell.org/package/hsexif-0.6.1.6/docs/Graphics-HsExif.html for keys
        getValue :: String -> (Map ExifTag ExifValue) -> String
        getValue "camera"   = intercalate " " . map (prettify fixEncoding) . findAll [make, model]
        getValue "lens"     = intercalate " " . map (prettify fixEncoding) . findAll [lensMake, lensModel]
        getValue "filmtype" = intercalate " " . map (prettify id)          . findAll [userComment]
        getValue "shutter"  = intercalate " " . map (prettify (T.replace " sec." "s")) .
                                                                             findAll [exposureTime]
        getValue "aperture" = intercalate " " . map (prettify (T.replace "f/" "ƒ/")) .
                                                                             findAll [fnumber]
        getValue "speed"    = intercalate " " . map (prettify id)          . findAll [isoSpeedRatings]
        getValue "copyright"= intercalate " " . map (prettify fixEncoding) . findAll [copyright]
        getValue "license"  = const "" -- TODO: read XMP tag cc:license
        getValue "location" = const "" -- TODO: read EXIF tag and do GPS lookup
        getValue "title"    = intercalate " " . map (prettify fixEncoding) . findAll [imageDescription]
        getValue tag        = error $ unwords ["Unknown EXIF field:", tag]
        -- Pretty-print an ExifValue given the ExifTag it belongs to
        prettify :: (T.Text -> T.Text) -> (ExifTag, ExifValue) -> String
        prettify f = T.unpack . f . uncurry prettyPrinter
        -- Fix encoding of string values by reinterpreting the string as UTF-8
        fixEncoding :: T.Text -> T.Text
        fixEncoding = (decodeUtf8 . DBC.pack . T.unpack)
        -- Keys not in hsexif
        lensMake  = ExifTag ExifSubIFD (Just "lensMake")  0xa433 $ T.pack . show
        lensModel = ExifTag ExifSubIFD (Just "lensModel") 0xa434 $ T.pack . show

photoExifField :: String -> Context a
photoExifField key = functionField key $ \[k] i -> do
    -- TODO: Is it possible to avoid the IO by getting a LBS from Hakyll instead?
    (\v -> if null v then empty else return v) $ getExifValue k . fromRight DMS.empty .
        unsafePerformIO . parseFileExif . toFilePath $ itemIdentifier i

exifKeyField :: String -> String -> Context a
exifKeyField key k = field key $ \i -> do
    -- TODO: Is it possible to avoid the IO by getting a LBS from Hakyll instead?
    (\v -> if null v then empty else return v) $ getExifValue k . fromRight DMS.empty .
        unsafePerformIO . parseFileExif . toFilePath $ itemIdentifier i

--------------------------------------------------------------------------------
photoContext :: Context String
photoContext =
    functionField "url"
        (\[v] -> fmap (maybe empty toUrl) . getRoute . setVersion (Just v) . itemIdentifier) <>
    photoFrameField "frame"           <>
    photoRollField  "roll"            <>
    photoDateField  "date" "%Y-%m-%d" <>
    photoExifField  "exif"            <>
    exifKeyField    "title" "title"   <>
    defaultContext'

collectionContext :: Context String
collectionContext =
    field "updated"
        (fmap (maybe empty (photoDate "%Y-%m-%d" . fst) . uncons) . photos) <>
    listFieldWith "photos"   photoContext      photos                   <>
    listFieldWith "showcase" photoContext      (fmap (take 1) . photos) <>
    defaultContext'
    where
        collectionTiffs = fromRegex . (\p -> (takeDirectory p) </> ".+\\.(tif|tiff|jpg|jpeg)") . toFilePath
        photos = fmap (reverse . chronological) . (photosFrom . collectionTiffs . itemIdentifier)
        photoDate fmt = formatTime defaultTimeLocale fmt . getPhotoItemDate . itemIdentifier

archiveContext :: Context String
archiveContext =
    listField "collections" collectionContext collections              <>
    listField "photos"      photoContext      photos                   <>
    listField "showcase"    photoContext      (fmap (take 1) $ photos) <>
    defaultContext'
    where
        collections = alphabetical <$> loadAllSnapshots (fromGlob "collections/*/index.*") "raw"
        photos = fmap (reverse . chronological) $ (photosFrom photoPathRegex)

defaultContext' :: Context String
defaultContext' =
    constField "root" root <>
    snippetField <>
    defaultContext


--------------------------------------------------------------------------------
photosFrom :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
photosFrom p = loadAll (p .&&. hasNoVersion)


--------------------------------------------------------------------------------
chronological :: [Item a] -> [Item a]
chronological = sortOn $ getPhotoItemDate . itemIdentifier

alphabetical :: [Item a] -> [Item a]
alphabetical = sortOn $ takeBaseName . toFilePath . itemIdentifier
