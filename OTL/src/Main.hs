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
{-# LANGUAGE OverloadedStrings, GADTs #-}

module Main (
    main
) where

import Text.OTL
import Text.OTL.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8

import Text.ParserCombinators.Parsec (ParseError)
import System.Exit (exitFailure)
import qualified Text.Pandoc as Pandoc
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B


main :: IO ()
main = do
  stdin <- getContents
  handleParse $ parse "<stdin>" stdin

handleParse :: Either ParseError Outline -> IO ()
handleParse (Left err) = print err >> exitFailure
handleParse (Right outline) = do
    args <- getArgs
    let outputFormat = case args of
                        [format] -> format
                        [] -> "html"
                        _ -> error $ "bad args " ++ show args
    let writer = fromMaybe (error $ "can't write " ++ outputFormat) $ lookup outputFormat Pandoc.writers
    options <- defaultWriterOptions outputFormat
    pandoc <- toPandoc outline
    let writeBinary :: B.ByteString -> IO ()
        writeBinary = B.writeFile (UTF8.encodePath "-")
    case writer of
      Pandoc.PureStringWriter w -> UTF8.putStr $ w options pandoc
      Pandoc.IOStringWriter w -> w options pandoc >>= UTF8.putStr
      Pandoc.IOByteStringWriter w -> w options pandoc >>= writeBinary
