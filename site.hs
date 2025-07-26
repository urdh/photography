{-# LANGUAGE OverloadedStrings #-}

import           Data.List        (isSuffixOf)
import           Hakyll
import           Hakyll.Images    (ensureFitCompiler, loadImage, compressJpgCompiler)
import           Helpers.Contexts (archiveContext, collectionContext, photoContext)

--------------------------------------------------------------------------------
root :: String
root = "https://photography.sigurdhsson.org"

config :: Configuration
config = defaultConfiguration {deployCommand = "vercel deploy _site/"}

extensions :: String
extensions = ".+\\.(tif|tiff|jpg|jpeg)"

photoPaths :: Pattern
photoPaths = fromRegex $ "collections/.+/" ++ extensions

indexPaths :: Pattern
indexPaths = fromRegex "collections/.+/index\\.[a-z]+"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config  $ do
  -- Collections
  match "collections/*/index.*" $ do
    route   $ setExtension "html"
    compile $
      pandocCompiler
        >>= saveSnapshot "raw"
        >>= loadAndApplyTemplate "templates/collection.html" collectionContext'
        >>= loadAndApplyTemplate "templates/default.html"    rootContext
        >>= cleanIndexUrls

  -- Photographies in a collection
  match photoPaths $
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/photo.html" photoContext'
        >>= cleanIndexUrls
  match photoPaths $ version "480px"  $ resize "jpeg" 480
  match photoPaths $ version "800px"  $ resize "jpeg" 800
  match photoPaths $ version "960px"  $ resize "jpeg" 960

  -- Style sheets and assets
  match "*.css" $ do
    route   $ idRoute
    compile $ compressCssCompiler

  -- Index, 404 page
  match ("index.html" .||. "404.html") $ do
    route   $ idRoute
    compile $
      getResourceBody
        >>= applyAsTemplate                               rootContext
        >>= loadAndApplyTemplate "templates/default.html" rootContext
        >>= cleanIndexUrls

  -- Recent photos JSON
  create ["photos.json"] $ do
      route   $ idRoute
      compile $
          makeItem ""
              >>= loadAndApplyTemplate "templates/photos.json" archiveContext'
              >>= cleanIndexUrls

  -- Sitemap
  create ["sitemap.xml"] $ do
    route   $ idRoute
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" archiveContext'
        >>= cleanIndexUrls

  -- Templates
  match "templates/sidebar.html" $
    compile $
      getResourceBody
        >>= applyAsTemplate archiveContext'
        >>= cleanIndexUrls
  match "templates/*" $ compile templateBodyCompiler

  -- robots.txt and vercel.json files
  match ("robots.txt" .||. "vercel.json") $ do
    route   $ idRoute
    compile $ copyFileCompiler

--------------------------------------------------------------------------------
resize :: String -> Int -> Rules ()
resize ext h = do
  route $ setExtension $ show h ++ "px." ++ ext
  compile $
    loadImage
      >>= compressJpgCompiler (100 :: Integer)
      >>= ensureFitCompiler 65535 h

--------------------------------------------------------------------------------
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls =
  return . fmap (withUrls (foldr ((.) . stripSuffix) id index'))
  where
    index' = ["index.html", "index.htm"]

stripSuffix :: String -> String -> String
stripSuffix suffix' str
  | suffix' `isSuffixOf` str = take (length str - length suffix') str
  | otherwise = str

--------------------------------------------------------------------------------
photoContext' :: Context String
photoContext' = photoContext rootContext indexPaths extensions

collectionContext' :: Context String
collectionContext' = collectionContext rootContext indexPaths extensions

archiveContext' :: Context String
archiveContext' = archiveContext rootContext indexPaths photoPaths extensions

rootContext :: Context String
rootContext =
  constField "root" root <>
  snippetField           <>
  defaultContext
