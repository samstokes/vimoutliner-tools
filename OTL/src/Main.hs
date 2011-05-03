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

import Text.OTL

import Text.Blaze.Html5 (docTypeHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8
import Text.ParserCombinators.Parsec (ParseError)
import qualified Data.ByteString.Lazy as BS
import System.Exit (exitFailure)
import Control.Monad (unless, forM_)
import qualified Text.Pandoc as Pandoc
import Data.Char (toLower)

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
htmlOutline outline = docTypeHtml $ do
    let titleItem = getOutlineTitleItem outline
    let title = getItemTitle titleItem
    H.head $ do
        H.title $ H.toHtml title
        stylesheet "style.css"
    H.body $ do
        H.div ! A.class_ "DocTitle" $ do
            H.h1 $ H.toHtml title
        H.div ! A.class_ "MainPage" $ do
            H.ol $ do
                forM_ (getItemChildren titleItem) $ renderItem 1
                forM_ (getOutlineNonTitleItems outline) $ renderItem 1
        H.div ! A.class_ "Footer" $ "Insert footer here"

stylesheet url = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href url

renderItem :: Int -> Item -> H.Html
renderItem depth (Item content items) = do
    H.li ! depthClass depth "L" $ do
        renderItemContent depth content
        unless (null items) $ H.ol $ do
            forM_ items $ renderItem (succ depth)

renderItemContent :: Int -> ItemContent -> H.Html
renderItemContent _ (Heading text) = H.toHtml text
renderItemContent depth (Body paragraphs) = forM_ paragraphs $ renderParagraph depth
renderItemContent depth (Preformatted content) = H.pre ! depthClass depth "PRE" $ H.toHtml content
renderItemContent depth (Table rows) = H.table ! depthClass depth "TAB" $ do
    H.tbody $ forM_ rows renderTableRow
renderItemContent depth (UserDef type_ content) = case getReader of
    Just reader -> H.preEscapedString $ writer pandoc
        where
        writer = Pandoc.writeHtmlString Pandoc.defaultWriterOptions
        pandoc = reader Pandoc.defaultParserState content
    Nothing -> H.pre ! A.title (H.toValue $ show type_) $ H.toHtml content
    where getReader = do -- Maybe monad
            type_' <- type_
            lookup (map toLower type_') Pandoc.readers
renderItemContent depth (PreUserDef type_ content) = H.pre ! A.title (H.toValue $ show type_) $ H.toHtml content

renderParagraph :: Int -> String -> H.Html
renderParagraph depth = (H.p ! depthClass depth "P") . H.toHtml

renderTableRow :: TableRow -> H.Html
renderTableRow (TableRow isHeader entries) = H.tr $ forM_ entries renderEntry
    where
    renderEntry = entryTag . H.toHtml
    entryTag = if isHeader then H.th else H.td

depthClass :: Int -> String -> H.Attribute
depthClass depth label = A.class_ $ H.toValue $ label ++ show depth
