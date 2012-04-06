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
    toPandoc
  , defaultWriterOptions
) where


import Text.OTL.Types
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Builder as P
import qualified Text.Pandoc.Shared as PS
import Data.Monoid (Monoid(..))
import Data.Foldable (Foldable(..))
import Data.Char (toLower)
import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)


toPandoc :: Outline -> Pandoc
toPandoc outline = P.setTitle outlineTitle $ P.doc $
    foldMap (itemToBlocks 1) (titleChildren ++ nonTitleItems)
    where
      titleItem = getOutlineTitleItem outline
      outlineTitle = P.text $ getHeading titleItem
      titleChildren = getHeadingChildren titleItem
      nonTitleItems = getOutlineNonTitleItems outline


defaultWriterOptions :: String -> IO P.WriterOptions
defaultWriterOptions outputFormat = do
    templateFile <- PS.readDataFile Nothing $ "templates/default." ++ outputFormat
    return P.defaultWriterOptions {
        P.writerStandalone = True
      , P.writerTemplate = templateFile
      }
  `catch` \_ -> return P.defaultWriterOptions { P.writerStandalone = True }


itemToBlocks :: Int -> Item -> P.Blocks
itemToBlocks level (Heading heading children) | level < 3 =
    (P.header level . P.text) heading `mappend`
    foldMap (itemToBlocks $ succ level) children
itemToBlocks level (Heading heading []) = itemToBlocks level (Body [heading])
itemToBlocks level (Heading heading children) =
    (P.header level . P.text) heading `mappend`
    P.orderedList (map (itemToBlocks $ succ level) children)
itemToBlocks _ (Body paragraphs) = foldMap (P.para . P.text) paragraphs
itemToBlocks _ (Preformatted content) = P.codeBlock content
itemToBlocks _ (Table []) = error "empty table"
itemToBlocks _ (Table rows@(headerRow : nonHeaderRows)) | isRowHeader headerRow =
    simpleTable (rowToBlocks headerRow) $ map rowToBlocks nonHeaderRows
                                          | otherwise =
    verySimpleTable $ map rowToBlocks rows
itemToBlocks level (UserDef type_ content) = case getReader type_ of
    Just reader -> nested $ reader P.defaultParserState (unlines content)
    Nothing -> itemToBlocks level $ Body (linesToParagraphs content)
itemToBlocks _ (PreUserDef type_ content) = P.codeBlockWith ("", maybeToList type_, []) content


getReader :: Maybe String -> Maybe (P.ParserState -> String -> Pandoc)
getReader type_ = do
    format <- map toLower <$> type_
    lookup format P.readers

nested :: Pandoc -> P.Blocks
nested (P.Pandoc _ blocks) = P.fromList blocks


rowToBlocks :: TableRow -> [P.Blocks]
rowToBlocks = map (P.plain . P.text) . getRowEntries


-- | A simple table without a caption.
-- Bug fix for Text.Pandoc.Builder.simpleTable which produces empty tables due
-- to an empty (alignment, width) list.
simpleTable :: [P.Blocks]   -- ^ Headers
            -> [[P.Blocks]] -- ^ Rows
            -> P.Blocks
simpleTable headers = P.table emptyCaption (mapConst defaultAlignWidth headers) headers
    where
        emptyCaption = P.empty
        defaultAlignWidth = (P.AlignDefault, 0)


-- | A very simple table without a caption and with autonumbered "Column n" headers.
verySimpleTable :: [[P.Blocks]] -> P.Blocks
verySimpleTable [] = simpleTable [] []
verySimpleTable rows@(row : _) = simpleTable autoHeaders rows
    where
        autoHeaders = map (autoHeader . snd) $ zip row ([1..] :: [Integer])
        autoHeader n = (P.plain . P.text) $ "Column " ++ show n


mapConst :: Functor f => b -> f a -> f b
mapConst k = fmap (const k)
