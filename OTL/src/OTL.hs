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
  , TableRow(..)
  , parser
  , parse
) where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.String
import Text.Parsec.Indent
import Data.Char (isSpace)


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
                 | Table { getTableRows :: [TableRow] }
                 | UserDef { getUserDefType :: Maybe String
                           , getBodyLines :: [String]
                           }
                 | PreUserDef { getUserDefType :: Maybe String
                              , getPreformattedContent :: String
                              }
  deriving (Show)

data TableRow = TableRow { isRowHeader :: Bool
                         , getRowEntries :: [String]
                         }
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
           <|> tableP
           <|> userDefP
           <|> preUserDefP
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

tableP :: ParserT ItemContent
tableP = (Table . map parseTableRow) <$> nonHeadingP '|'

userDefP :: ParserT ItemContent
userDefP = makeUserDef <$> nonHeadingP '>'
    where
    makeUserDef (defn : rest) | not (isSpace (head defn)) = UserDef (Just defn) (mungeBody rest)
    makeUserDef textLines = UserDef Nothing (mungeBody textLines)
    mungeBody = map lstrip1

preUserDefP :: ParserT ItemContent
preUserDefP = makePreUserDef <$> nonHeadingP '<'
    where
    makePreUserDef (defn : rest) | not (isSpace (head defn)) = PreUserDef (Just defn) $ unlines rest
    makePreUserDef textLines = PreUserDef Nothing $ unlines textLines

parseTableRow :: String -> TableRow
parseTableRow line = case Parsec.parse tableRowP "table row" line of
    Right row -> row
    Left err -> error $ "couldn't parse table row (" ++ line ++ "): " ++ show err

tableRowP :: Parsec String () TableRow
tableRowP = TableRow <$> isHeadingP <*> entriesP
    where
    isHeadingP = presence $ char '|'
    entriesP = sepEndBy1 (spaced $ many1 tableEntryCharP) (char '|')


nonHeadingP :: Char -> ParserT [String]
nonHeadingP startChar = block (char startChar *> lineP <* newline <* spaces)

lineP :: ParserT String
lineP = many lineCharP

nonEmptyLineP :: ParserT String
nonEmptyLineP = many1 lineCharP

lineCharP :: Monad m => ParsecT String u m Char
lineCharP = noneOf "\n"

tableEntryCharP :: Monad m => ParsecT String u m Char
tableEntryCharP = noneOf "|\n"

presence :: ParsecT s u m a -> ParsecT s u m Bool
presence p = (p *> return True) <|> return False

spaced :: Monad m => ParsecT String u m a -> ParsecT String u m a
spaced p = spaces *> p <* spaces

lstrip1 :: String -> String
lstrip1 = dropIf isSpace

dropIf :: (Char -> Bool) -> String -> String
dropIf p (c : cs) | p c = cs
dropIf _ cs = cs
