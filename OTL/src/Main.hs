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
import Control.Monad (forM_)

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
htmlOutline (Outline items) = docTypeHtml $ do
    let (Item (TextContent title) _) = head items
    H.head $ do
        H.title $ H.toHtml title
        stylesheet "style.css"
    H.body $ do
        H.div ! A.class_ "DocTitle" $ do
            H.h1 $ H.toHtml title
        H.div ! A.class_ "MainPage" $ do
            forM_ items (H.p . renderItem 1)
        H.div ! A.class_ "Footer" $ "Insert footer here"
    H.preEscapedString "<!--"
    H.preEscapedString $ show items
    H.preEscapedString "-->"

stylesheet url = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href url

renderItem :: Int -> Item -> H.Html
renderItem depth (Item (TextContent text) items) = do
    H.toHtml text
    H.ol $ do
        forM_ items (\item -> H.li ! A.class_ liClass $ renderItem (succ depth) item)
    where
        liClass = H.toValue $ "L" ++ show depth
