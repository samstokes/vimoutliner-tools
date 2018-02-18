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


-- StylePresentation - Heuristics to allow mixing headers and text on a slide
-- while balancing the confusion of too many header styles
itemToBlocks :: Style -> Int -> Item -> IO P.Blocks
itemToBlocks StylePresentation level (Heading heading children) | level < 3 = mappend <$>
    pure ((P.header level . P.text) heading) <*>
    foldMapM (itemToBlocks StylePresentation $ succ level) children
itemToBlocks StylePresentation level (Heading heading []) = itemToBlocks StylePresentation level (Body [heading])
itemToBlocks StylePresentation level (Heading heading children) = mappend <$>
    pure ((P.header level . P.text) heading) <*>
    foldMapM (itemToBlocks StylePresentation $ succ level) children

-- StyleNotes - preserve the structure of the outline, render nested bullets
itemToBlocks StyleNotes level (Heading heading children) = mappend <$>
    pure (notesHeader level heading) <*> notesChildren level children
  where
    -- headers start at h2 since the template usually renders the title as h1
    notesHeader 1 = P.header 2 . P.text
    notesHeader _ = P.plain . P.text
    notesChildren _ [] = pure mempty
    notesChildren level' children' = P.bulletList <$>
        mapM (itemToBlocks StyleNotes (level' + 1)) children'

-- "leaf" items, common between styles
itemToBlocks _ _ (Body paragraphs) = pure $ foldMap (P.para . P.text) paragraphs
itemToBlocks _ _ (Preformatted content) = pure $ P.codeBlock content
itemToBlocks _ _ (UserDef (Just (UserDefType "COMMENT" _)) _) = pure mempty
itemToBlocks _ _ (UserDef (Just (UserDefType "TODO" _)) _) = pure mempty
itemToBlocks style level (UserDef (Just (UserDefType type_ classes)) content) = case getReader type_ of
    Just reader -> nested classes <$> reader def (unlines content)
    Nothing -> itemToBlocks style level $ Body (linesToParagraphs content)
itemToBlocks style level (UserDef Nothing content) =
    itemToBlocks style level $ Body (linesToParagraphs content)
itemToBlocks _ _ (Table []) = error "empty table"
itemToBlocks _ _ (Table rows@(headerRow : nonHeaderRows)) | isRowHeader headerRow = pure $
    P.simpleTable (rowToBlocks headerRow) $ map rowToBlocks nonHeaderRows
                                          | otherwise = pure $
    verySimpleTable $ map rowToBlocks rows
itemToBlocks _ _ (PreUserDef (Just (UserDefType "IMAGE" classes)) content) = pure $ P.plain $ P.imageWith attrs url ("title is " ++ url) (P.text $ "alt is " ++ url)
  where url = takeUntilFirst '\n' content
        attrs = ("", classes, [])
itemToBlocks _ _ (PreUserDef type_ content) = pure $ P.codeBlockWith (typeToAttr type_) content
  where
    typeToAttr (Just (UserDefType codeType classes)) = ("", codeType : classes, [])
    typeToAttr Nothing = P.nullAttr


getReader :: String -> Maybe (P.ReaderOptions -> String -> IO Pandoc)
getReader type_ = do
    let format = map toLower type_
    stringReader <$> lookup format P.readers
  where stringReader (P.StringReader reader) = handlingParseErrors reader
        stringReader (P.ByteStringReader _) = error $ "got ByteStringReader for type " ++ show type_
        handlingParseErrors reader opts str =
            either (error . show) id <$> reader opts str

nested :: [String] -> Pandoc -> P.Blocks
nested classes (P.Pandoc _ blocks) = P.divWith ("", classes, []) $ P.fromList blocks


takeUntilFirst :: Eq a => a -> [a] -> [a]
takeUntilFirst c = takeWhile (/= c)


rowToBlocks :: TableRow -> [P.Blocks]
rowToBlocks = map (P.plain . P.text) . getRowEntries


-- | A very simple table without a caption and with autonumbered "Column n" headers.
verySimpleTable :: [[P.Blocks]] -> P.Blocks
verySimpleTable [] = P.simpleTable [] []
verySimpleTable rows@(row : _) = P.simpleTable emptyHeaders rows
    where
        emptyHeaders = map (const emptyHeader) row
        emptyHeader = mempty
