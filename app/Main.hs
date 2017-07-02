{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc (HTMLMathMethod(..), WriterOptions(..))
import Data.Char


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Types of Note"
    , feedDescription = "Notes on type theory, theorem proving, and formal verification."
    , feedAuthorName  = "Calvin Beck"
    , feedAuthorEmail = "hobbes@ualberta.ca"
    , feedRoot        = "https://www.typesofnote.com"
    }

writerOptions = defaultHakyllWriterOptions {writerHTMLMathMethod = MathML Nothing}


main :: IO ()
main = hakyll $ do      
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "contact.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" contactCtx
            >>= relativizeUrls

    match "about.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" aboutCtx
            >>= relativizeUrls


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" (homeCtxWithTags tags)
                >>= saveSnapshot "feedContent"
                >>= loadAndApplyTemplate "templates/default.html" (homeCtxWithTags tags)
                >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
                >>= loadAndApplyTemplate "templates/post.html" (homeCtxWithTags tags)
                >>= loadAndApplyTemplate "templates/default.html" (homeCtxWithTags tags)
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    archiveCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archCtx
                >>= loadAndApplyTemplate "templates/default.html" archCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" (teaserCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    homeCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "feedContent"
            renderAtom myFeedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "feedContent"
            renderRss myFeedConfiguration feedCtx posts

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged with \"" ++ tag ++ "\"."
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

homeCtxWithTags :: Tags -> Context String
homeCtxWithTags tags = tagsField "tags" tags `mappend` homeCtx

feedCtx :: Context String
feedCtx = postCtx `mappend` bodyField "description"

archiveCtx = constField "archive" "" `mappend` postCtx
homeCtx = constField "home" "" `mappend` postCtx
aboutCtx = constField "about" "" `mappend` postCtx
contactCtx = constField "contact" "" `mappend` postCtx

teaserCtx = teaserField "teaser" "content" `mappend` postCtx

teaserCtxWithTags :: Tags -> Context String
teaserCtxWithTags tags = tagsField "tags" tags `mappend` teaserCtx
