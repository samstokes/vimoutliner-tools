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

import Text.ParserCombinators.Parsec (ParseError)
import System.Exit (exitFailure)
import qualified Text.Pandoc as Pandoc
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)


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
    putStrLn $ writer options $ pandoc
