-----------------------------------------------------------------------------
--
-- Module      :  OTL
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

module OTL (
    Outline(..)
  , Item(..)
  , ItemContent(..)
  , parser
) where

import Control.Applicative hiding (many)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Indent
import Debug.Trace


{- REPRESENTATION -}

newtype Outline = Outline [Item]
  deriving (Show)

data Item = Item ItemContent [Item]
  deriving (Show)

data ItemContent = TextContent String
  deriving (Show)


{- PARSER STATE -}

data ParserState = ParserState {
                       getParserIndentation :: Int
                   }
initialState :: ParserState
initialState = ParserState { getParserIndentation = 0 }

type ParserT a = IndentParser String () a


{- PARSER -}

parser :: ParserT Outline
parser = outlineP

outlineP :: ParserT Outline
outlineP = Outline <$> many1 itemP

itemP :: ParserT Item
itemP = withBlock Item (itemContentP <* spaces) itemP

itemContentP :: ParserT ItemContent
itemContentP = TextContent <$> many1 (noneOf "\n") <* newline
