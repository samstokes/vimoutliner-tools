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
  , parse
) where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Indent


{- REPRESENTATION -}

newtype Outline = Outline { getOutlineItems :: [Item] }
  deriving (Show)

data Item = Item { getItemContent :: ItemContent
                 , getItemChildren :: [Item]
                 }
  deriving (Show)

data ItemContent = Heading { getHeading :: String }
                 | Body { getBodyParagraphs :: [String] }
                 | Preformatted { getPreformattedContent :: String }
  deriving (Show)



{- PARSER -}

type ParserT a = IndentParser String () a


parse :: SourceName -> String -> Either ParseError Outline
parse sourceName = runIndent sourceName . runParserT parser () sourceName

parser :: ParserT Outline
parser = outlineP

outlineP :: ParserT Outline
outlineP = Outline <$> many1 itemP

itemP :: ParserT Item
itemP = withBlock Item (itemContentP <* spaces) itemP

itemContentP :: ParserT ItemContent
itemContentP = bodyP
           <|> preformattedP
           <|> headingP

headingP :: ParserT ItemContent
headingP = Heading <$> nonEmptyLineP <* newline

bodyP :: ParserT ItemContent
bodyP = (Body . unlinesSplitByBlanks) <$> nonHeadingP ':'

unlinesSplitByBlanks :: [String] -> [String]
unlinesSplitByBlanks = map unlines . splitBy null

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = foldr addUnlessP []
    where
    addUnlessP item groups | p item = [] : groups
    addUnlessP item [] = [[item]]
    addUnlessP item (group : groups) = (item : group) : groups

preformattedP :: ParserT ItemContent
preformattedP = (Preformatted . unlines) <$> nonHeadingP ';'

nonHeadingP :: Char -> ParserT [String]
nonHeadingP startChar = block (char startChar *> lineP <* newline <* spaces)

lineP :: ParserT String
lineP = many lineCharP

nonEmptyLineP :: ParserT String
nonEmptyLineP = many1 lineCharP

lineCharP :: ParserT Char
lineCharP = noneOf "\n"
