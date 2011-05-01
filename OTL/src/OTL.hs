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

data ItemContent = TextContent { getText :: String }
                 | BodyContent { getBodyParagraphs :: [String] }
                 | PreformattedContent { getPreformattedContent :: String }
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
itemContentP = bodyContentP
           <|> preformattedContentP
           <|> textContentP

textContentP :: ParserT ItemContent
textContentP = TextContent <$> nonEmptyLineP <* newline

bodyContentP :: ParserT ItemContent
bodyContentP = BodyContent <$> paragraphs
    where
    paragraphs = colonLines >>= return . unlinesSplitByBlanks
    colonLines = block $ do
    char ':'
    line <- lineP
    newline >> spaces
    return line

unlinesSplitByBlanks :: [String] -> [String]
unlinesSplitByBlanks = map unlines . splitBy null

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = foldr addUnlessP []
    where
    addUnlessP item groups | p item = [] : groups
    addUnlessP item [] = [[item]]
    addUnlessP item (group : groups) = (item : group) : groups

preformattedContentP :: ParserT ItemContent
preformattedContentP = PreformattedContent <$> content
    where
    content = semicolonLines >>= return . unlines
    semicolonLines = block $ do
    char ';'
    line <- lineP
    newline >> spaces
    return line

lineP :: ParserT String
lineP = many lineCharP

nonEmptyLineP :: ParserT String
nonEmptyLineP = many1 lineCharP

lineCharP :: ParserT Char
lineCharP = noneOf "\n"
