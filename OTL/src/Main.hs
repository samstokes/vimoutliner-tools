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
import Text.Pandoc (WriterOptions(..))
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
    let (outputFormat, opts) = case args of
            format : rest -> (format, rest)
            [] -> error $ "bad args " ++ show args
    let writer = fromMaybe (error $ "can't write " ++ outputFormat) $ lookup outputFormat Pandoc.writers
    options <- defaultWriterOptions outputFormat
    let options' = case opts of
                [] -> options
                ["--self-contained"] ->
                    options { writerVariables = ("slidy-url", "slidy") : writerVariables options,
                              writerStandalone = True }
                _ -> error $ "bad options " ++ show opts
    let pandoc = toPandoc outline
    putStrLn $ writer options' $ pandoc
