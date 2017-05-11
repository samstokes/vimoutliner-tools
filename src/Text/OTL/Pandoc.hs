-----------------------------------------------------------------------------
--
-- Module      :  Text.OTL.Pandoc
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

module Text.OTL.Pandoc (
    Style(..), styleName, nameToStyle,
    toPandoc
  , defaultWriterOptions
) where


import Text.OTL.Types
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Builder as P
import qualified Text.Pandoc.Shared as PS
import Control.Exception
import Data.Default (def)
import Data.Foldable (foldrM)
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import Data.ByteString.Char8 (unpack)


data Style = StylePresentation | StyleNotes
  deriving (Read, Show, Eq)

styleName :: Style -> String
styleName StylePresentation = "presentation"
styleName StyleNotes = "notes"

nameToStyle :: String -> Either String Style
nameToStyle "presentation" = Right StylePresentation
nameToStyle "notes" = Right StyleNotes
nameToStyle name = Left $ "invalid style " ++ show name


foldMapM :: (Monoid monoid, Monad monad, Foldable t) => (a -> monad monoid) -> t a -> monad monoid
foldMapM f = foldrM appF mempty
  where
    appF a b = do
      fa <- f a
      return $ mappend fa b


toPandoc :: Style -> Outline -> IO Pandoc
toPandoc style outline = P.setTitle outlineTitle . P.doc <$>
    foldMapM (itemToBlocks style 1) (titleChildren ++ nonTitleItems)
    where
      titleItem = getOutlineTitleItem outline
      outlineTitle = P.text $ getHeading titleItem
      titleChildren = getHeadingChildren titleItem
      nonTitleItems = getOutlineNonTitleItems outline


defaultWriterOptions :: String -> IO P.WriterOptions
defaultWriterOptions outputFormat = do
    templateFile <- PS.readDataFile Nothing $ "templates/default." ++ outputFormat
    return def {
        P.writerStandalone = True
      , P.writerTemplate = unpack templateFile
      }
  `catch` ((\_ -> return def { P.writerStandalone = True }) :: SomeException -> IO P.WriterOptions)


itemToBlocks :: Style -> Int -> Item -> IO P.Blocks
itemToBlocks StylePresentation level (Heading heading children) | level < 3 = mappend <$>
    pure ((P.header level . P.text) heading) <*>
    foldMapM (itemToBlocks StylePresentation $ succ level) children
itemToBlocks StylePresentation level (Heading heading []) = itemToBlocks StylePresentation level (Body [heading])
itemToBlocks StylePresentation level (Heading heading children) = mappend <$>
    pure ((P.header level . P.text) heading) <*>
    foldMapM (itemToBlocks StylePresentation $ succ level) children

itemToBlocks StyleNotes 1 (Heading heading children) = mappend <$>
    pure ((P.header 2 . P.text) heading) <*>
    (P.bulletList <$> mapM (itemToBlocks StyleNotes 2) children)
itemToBlocks StyleNotes level (Heading heading children) = mappend <$>
    pure ((P.plain . P.text) heading) <*>
    (P.bulletList <$> mapM (itemToBlocks StyleNotes (level + 1)) children)

-- "leaf" items, common between styles
itemToBlocks _ _ (Body paragraphs) = pure $ foldMap (P.para . P.text) paragraphs
itemToBlocks _ _ (Preformatted content) = pure $ P.codeBlock content
itemToBlocks style level (UserDef type_ content) = case getReader type_ of
    Just reader -> nested <$> reader def (unlines content)
    Nothing -> itemToBlocks style level $ Body (linesToParagraphs content)
itemToBlocks _ _ (Table []) = error "empty table"
itemToBlocks _ _ (Table rows@(headerRow : nonHeaderRows)) | isRowHeader headerRow = pure $
    P.simpleTable (rowToBlocks headerRow) $ map rowToBlocks nonHeaderRows
                                          | otherwise = pure $
    verySimpleTable $ map rowToBlocks rows
itemToBlocks _ _ (PreUserDef (Just "IMAGE") content) = pure $ P.plain $ P.image url ("title is " ++ url) (P.text $ "alt is " ++ url)
  where url = takeUntilFirst '\n' content
itemToBlocks _ _ (PreUserDef type_ content) = pure $ P.codeBlockWith ("", maybeToList type_, []) content


getReader :: Maybe String -> Maybe (P.ReaderOptions -> String -> IO Pandoc)
getReader type_ = do
    format <- map toLower <$> type_
    stringReader <$> lookup format P.readers
  where stringReader (P.StringReader reader) = handlingParseErrors reader
        stringReader (P.ByteStringReader _) = error $ "got ByteStringReader for type " ++ show type_
        handlingParseErrors reader opts str =
            either (error . show) id <$> reader opts str

nested :: Pandoc -> P.Blocks
nested (P.Pandoc _ blocks) = P.fromList blocks


takeUntilFirst :: Eq a => a -> [a] -> [a]
takeUntilFirst c = takeWhile (/= c)


rowToBlocks :: TableRow -> [P.Blocks]
rowToBlocks = map (P.plain . P.text) . getRowEntries


-- | A very simple table without a caption and with autonumbered "Column n" headers.
verySimpleTable :: [[P.Blocks]] -> P.Blocks
verySimpleTable [] = P.simpleTable [] []
verySimpleTable rows@(row : _) = P.simpleTable autoHeaders rows
    where
        autoHeaders = map (autoHeader . snd) $ zip row ([1..] :: [Integer])
        autoHeader n = (P.plain . P.text) $ "Column " ++ show n
