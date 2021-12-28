--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll (
    Configuration (destinationDirectory),
    Context,
    applyAsTemplate,
    compile,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultConfiguration,
    defaultContext,
    getResourceBody,
    hakyllWith,
    idRoute,
    listField,
    loadAll,
    loadAllSnapshots,
    loadAndApplyTemplate,
    makeItem,
    match,
    pandocCompiler,
    recentFirst,
    relativizeUrls,
    route,
    setExtension,
    templateBodyCompiler,
 )
import Hakyll.Core.Compiler (saveSnapshot)
import Hakyll.Web.Template.Context (teaserField)

--------------------------------------------------------------------------------
config :: Configuration
config =
    defaultConfiguration
        { destinationDirectory = "docs"
        }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
        <> defaultContext

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "about.md" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let archiveCtx =
                    listField "posts" (teaserField "teaser" "content" <> postCtx) (return posts)
                        <> constField "title" "Archives"
                        <> postCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            recentPosts <- (fmap (take 10) . recentFirst) =<< loadAllSnapshots "posts/*" "content"
            let archiveCtx =
                    listField "posts" (teaserField "teaser" "content" <> postCtx) (return recentPosts)
                        <> constField "title" "Posts"
                        <> postCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
