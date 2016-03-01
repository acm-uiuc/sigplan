{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default
import           Data.Monoid
import           Hakyll

hconfig :: Configuration
hconfig = def { providerDirectory = "static" }

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

main :: IO ()
main = hakyllWith hconfig $ do
  match "CNAME" $ route idRoute >> compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.md", "contact.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = mconcat [ listField "posts" postCtx (return posts)
                               , constField "title" "Archives"
                               , defaultContext ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = mconcat [ listField "posts" postCtx (return posts)
                             , constField "title" "Home"
                             , defaultContext ]

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
