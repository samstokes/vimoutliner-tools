-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import OTL

import Text.Blaze.Html5 (docTypeHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8
import Text.ParserCombinators.Parsec (ParseError)
import qualified Data.ByteString.Lazy as BS
import System.Exit (exitFailure)
import Control.Monad (unless, forM_)

main :: IO ()
main = do
  stdin <- getContents
  handleParse $ parse "<stdin>" stdin

handleParse :: Either ParseError Outline -> IO ()
handleParse (Left err) = print err >> exitFailure
handleParse (Right outline) = printHtml outline

printHtml :: Outline -> IO ()
printHtml outline = BS.putStrLn $ renderHtml $ htmlOutline outline

htmlOutline :: Outline -> H.Html
htmlOutline (Outline (titleItem : items)) = docTypeHtml $ do
    let (Item (Heading title) titleChildren) = titleItem
    H.head $ do
        H.title $ H.toHtml title
        stylesheet "style.css"
    H.body $ do
        H.div ! A.class_ "DocTitle" $ do
            H.h1 $ H.toHtml title
        H.div ! A.class_ "MainPage" $ do
            H.ol $ do
                forM_ titleChildren $ renderItem 1
                forM_ items $ renderItem 1
        H.div ! A.class_ "Footer" $ "Insert footer here"
    H.preEscapedString "<!--"
    H.preEscapedString $ show items
    H.preEscapedString "-->"

stylesheet url = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href url

renderItem :: Int -> Item -> H.Html
renderItem depth (Item content items) = do
    H.li ! A.class_ liClass $ do
        renderItemContent content
        unless (null items) $ H.ol $ do
            forM_ items $ renderItem (succ depth)
    where
        liClass = H.toValue $ "L" ++ show depth

renderItemContent :: ItemContent -> H.Html
renderItemContent (Heading text) = H.toHtml text
renderItemContent (Body paragraphs) = forM_ paragraphs (H.p . H.toHtml)
renderItemContent (Preformatted content) = H.pre $ H.toHtml content
