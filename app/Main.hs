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


import qualified Text.ParserCombinators.Parsec as Parsec
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Options.Applicative as O
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.OTL
import Text.OTL.Pandoc
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.UTF8 as UTF8


data Options = Options {
    optsOutputFormat :: String
  , optsOutlineStyle :: Style
  }


main :: IO ()
main = do
  opts <- O.execParser $ O.info (O.helper <*> parseOptions) O.fullDesc
  writeFunc <- lookupWriteFunc $ optsOutputFormat opts
  stdin <- getContents
  case parse "<stdin>" stdin of
    Left err -> print err >> exitFailure
    Right outline -> toPandoc (optsOutlineStyle opts) outline >>= writeFunc

parseOptions :: O.Parser Options
parseOptions = Options <$>
      O.strOption (O.value "html" <> O.long "output-format" <> O.short 'f'
        <> O.metavar "FORMAT" <> O.help "Select output format (known to pandoc) - default html")
  <*> O.option readStyle (O.value StyleNotes <> O.long "outline-style" <> O.short 's'
       <> O.metavar "STYLE" <> O.help "Select outline style - notes (default) or presentation")

readStyle :: O.ReadM Style
readStyle = do
  val <- O.str
  case val of
    "notes" -> return StyleNotes
    "presentation" -> return StylePresentation
    _ -> O.readerError $ "invalid style '" ++ val ++ "'"

lookupWriteFunc :: String -> IO (Pandoc.Pandoc -> IO ())
lookupWriteFunc outputFormat = do
    let mWriter = lookup outputFormat Pandoc.writers
    writerOptions <- withHighlight <$> defaultWriterOptions outputFormat
    return $ case mWriter of
      Just (Pandoc.PureStringWriter w) -> UTF8.putStr . w writerOptions
      Just (Pandoc.IOStringWriter w) -> w writerOptions >=> UTF8.putStr
      Just (Pandoc.IOByteStringWriter w) -> w writerOptions >=> writeBinary
      Nothing -> error $ "can't write " ++ outputFormat
  where
    withHighlight options = options { Pandoc.writerHighlight = True }
    writeBinary :: B.ByteString -> IO ()
    writeBinary = B.writeFile (UTF8.encodePath "-")
