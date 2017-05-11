-----------------------------------------------------------------------------
--
-- Module      :  Text.OTL.Parser
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

module Text.OTL.Parser (
    parser
  , parse
) where



import Text.OTL.Types

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.String () -- just want Stream instance
import Text.Parsec.Indent
import Data.Char (isSpace)


type ParserT a = IndentParser String () a


parse :: SourceName -> String -> Either ParseError Outline
parse name = runIndent name . runParserT parser () name

parser :: ParserT Outline
parser = outlineP

outlineP :: ParserT Outline
outlineP = Outline <$> many1 itemP

itemP :: ParserT Item
itemP = bodyP
    <|> preformattedP
    <|> tableP
    <|> userDefP
    <|> preUserDefP
    <|> headingP

headingP :: ParserT Item
headingP = withBlock Heading (nonEmptyLineP <* newline <* spaces) itemP

bodyP :: ParserT Item
bodyP = (Body . linesToParagraphs) <$> nonHeadingP ':'

preformattedP :: ParserT Item
preformattedP = (Preformatted . unlines) <$> nonHeadingP ';'

tableP :: ParserT Item
tableP = Table <$> do
  rows <- nonHeadingP '|'
  mapM (subparse "table row" tableRowP) rows

userDefP :: ParserT Item
userDefP = makeUserDef <$> nonHeadingP '>'
    where
    makeUserDef (defn : rest) | not (isSpace (head defn)) = UserDef (Just defn) (mungeBody rest)
    makeUserDef textLines = UserDef Nothing (mungeBody textLines)
    mungeBody = map lstrip1

preUserDefP :: ParserT Item
preUserDefP = makePreUserDef <$> nonHeadingP '<'
    where
    makePreUserDef (defn : rest) | not (isSpace (head defn)) = PreUserDef (Just defn) $ unlines rest
    makePreUserDef textLines = PreUserDef Nothing $ unlines textLines

subparse :: String -> Parsec String () a -> String -> ParserT a
subparse sublabel p = either subfail return . Parsec.parse p ""
  where subfail err = parserFail $ "couldn't parse " ++ sublabel ++ ": " ++ show err

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
