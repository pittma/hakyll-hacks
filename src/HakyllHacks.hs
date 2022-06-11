{-# LANGUAGE OverloadedStrings #-}

module HakyllHacks
    ( asTempWithDefault
    , dateCtx
    , dateSlug
    , hakyllWithBaseRules
    , toIdxPath
    , slugRoute
    ) where

import Data.List.Split (splitOn)
import Hakyll
import System.FilePath


-- | Sets up the hakyll monad with some default rules for copying css,
-- | images, js, and files.
hakyllWithBaseRules :: Rules () -> IO ()
hakyllWithBaseRules rules = hakyll (baseRules >> rules)

baseRules :: Rules ()
baseRules = do
  match "css/*.css" $ do
    route idRoute
    compile copyFileCompiler
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler
  match "files/*" $ do
    route idRoute
    compile copyFileCompiler
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler 
  match "templates/*" $ compile templateCompiler

 -- | Loads the given identifier as a template with the given context.
asTempWithDefault :: Identifier -> Context String -> Compiler (Item String)
asTempWithDefault tn cs  =
  getResourceBody >>= applyAsTemplate cs >>=
  loadAndApplyTemplate tn defaultContext

-- | Sticks a date field into the context in the `%m-%d-%Y` format.
dateCtx :: Context String
dateCtx = dateField "date" "%m-%d-%Y"

-- | This is the infamous `niceRoute' function, it turns a route into
-- | a path that finishes with `index.html'
toIdxPath :: Routes
toIdxPath = customRoute createIndexRoute
  where
    createIndexRoute ident =
      let path = toFilePath ident
       in takeDirectory path </> takeBaseName path </> "index.html"

-- | Takes an identifier (file name), in the yyyy-mm-dd-{dashed-title}
-- | format and converts it to a slug path.
slugRoute :: Routes
slugRoute = customRoute dateSlug

-- | Creates a dated slug from an identifier.
dateSlug :: Identifier -> FilePath
dateSlug ident =
  let path = toFilePath ident
      fileNameSplit = splitOn "-" (takeBaseName path)
   in takeDirectory path </> head fileNameSplit </> fileNameSplit !! 1 </>
      drop 11 (takeBaseName path) </>
      "index.html"
