--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Control.Monad ((<=<))
import           Data.List (stripPrefix, dropWhileEnd, intercalate)
import           Data.ByteString.Lazy as L (ByteString)
import           Data.List.Split (splitOn)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
 
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
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

    match "course-info/*/*/index.markdown" $ do
        route $ composeRoutes (gsubRoute "course-info/" $ const "") (setExtension "html")
        compile $ do
          let courseDesc = "a student-organized group theory course at the UW during Winter 2019."
          pandocCompiler
            >>= loadAndApplyTemplate "templates/course-info.html" (courseInfoCtx courseDesc)
            >>= relativizeUrls

    match "course-info/*/*/syllabus/syllabus.tex" $ do
      route $ gsubRoute "/syllabus/" (const  "/")
              `composeRoutes` gsubRoute "course-info/" (const "")
              `composeRoutes` setExtension "pdf"
      compile latexCompiler

    -- remove this
    match "course-info/algebra/winter2019/problem_set1.pdf" $ do
      route $ gsubRoute "course-info/" $ const ""
      compile copyFileCompiler
    
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

latexCompiler :: Compiler (Item L.ByteString)
latexCompiler = do
  latexFileItem <- getResourceLBS
  let path = toFilePath $ itemIdentifier latexFileItem
  case reverse $ splitOn "/" path of
    (latexFile:latexDir:backwards_components) -> do
      let extraFilesDir = intercalate "/" $ reverse (latexDir : backwards_components)
      let pdfDir = intercalate "/" $ reverse backwards_components
      let pdfFileName = takeWhile (/= '.') latexFile ++ ".pdf"
      item <- withItemBody (unixFilterLBS "pdflatex" ["-output-directory=" ++ extraFilesDir, path]) latexFileItem
      pdf <- withItemBody (unixFilterLBS "cat" [extraFilesDir ++ "/" ++ pdfFileName]) item
      _ <- withItemBody (unixFilterLBS "rm" [extraFilesDir ++ "/" ++ pdfFileName]) item
      return pdf
    [] -> error "latexCompiler: Latex file must be in a directory structure like dir/latexFiles/*.latex"

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

courseInfoCtx :: String -> Context String
courseInfoCtx courseDesc =
  defaultContext `mappend`
  constField "class" courseDesc
