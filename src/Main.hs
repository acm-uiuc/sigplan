{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default
import           Data.Monoid
import           Data.Typeable
import           Hakyll
import           Text.Highlighting.Kate.Styles (zenburn)
import           Text.Pandoc.Options

hconfig :: Configuration
hconfig = def { providerDirectory = "static" }

minutesCtx :: Context String
minutesCtx = dateField "date" "%B %e, %Y" <> defaultContext

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith ro wo
  where
    ro :: ReaderOptions
    ro = def

    wo :: WriterOptions
    wo = def { writerHighlight      = False
             , writerHighlightStyle = zenburn
             , writerHtml5          = True
             }

minutesList :: Compiler [Item String]
minutesList = recentFirst =<< loadAll "minutes/*"

minutesF :: Maybe Int -> Context String
minutesF m = listField "minutes" minutesCtx $ getN <$> minutesList
  where
    getN = case m of Nothing -> id
                     Just n  -> take n

titleF :: String -> Context String
titleF = constField "title"

addToDefault :: [Context String] -> Context String
addToDefault cs = mconcat (cs <> [defaultContext])

main :: IO ()
main = hakyllWith hconfig $ do
  let matchFile path = match path $ route idRoute >> compile copyFileCompiler

  matchFile "CNAME"
  matchFile "favicon.ico"
  matchFile "images/*"
  matchFile "js/*"

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  let topLevelMarkdown = ["projects.md", "minutes.md", "meetings.md"]

  match (fromList topLevelMarkdown) $ do
    route   $ setExtension "html"
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "minutes/*" $ do
    route $ setExtension "html"
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/minutes.html" minutesCtx
      >>= loadAndApplyTemplate "templates/default.html" minutesCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = addToDefault [ minutesF Nothing
                                    , titleF "Meeting Minutes" ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      let indexCtx = addToDefault [ minutesF (Just 5), titleF "Home" ]
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= renderPandoc
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
