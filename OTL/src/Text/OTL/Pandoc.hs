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
import Control.Exception
import Data.Default (def)
import Data.Monoid (Monoid(..))
import Data.Foldable (Foldable(..), foldrM)
import Data.Char (toLower)
import Control.Applicative
import Data.Maybe (maybeToList)
import Data.ByteString.Char8 (unpack)


foldMapM :: (Monoid monoid, Monad monad, Foldable t) => (a -> monad monoid) -> t a -> monad monoid
foldMapM f = foldrM appF mempty
  where
    appF a b = do
      fa <- f a
      return $ mappend fa b


toPandoc :: Outline -> IO Pandoc
toPandoc outline = P.setTitle outlineTitle . P.doc <$>
    foldMapM (itemToBlocks 1) (titleChildren ++ nonTitleItems)
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


itemToBlocks :: Int -> Item -> IO P.Blocks
itemToBlocks level (Heading heading children) | level < 3 = mappend <$>
    pure ((P.header level . P.text) heading) <*>
    foldMapM (itemToBlocks $ succ level) children
itemToBlocks level (Heading heading []) = itemToBlocks level (Body [heading])
itemToBlocks level (Heading heading children) = mappend <$>
    pure ((P.header level . P.text) heading) <*>
    foldMapM (itemToBlocks $ succ level) children
itemToBlocks _ (Body paragraphs) = pure $ foldMap (P.para . P.text) paragraphs
itemToBlocks _ (Preformatted content) = pure $ P.codeBlock content
itemToBlocks _ (Table []) = error "empty table"
itemToBlocks _ (Table rows@(headerRow : nonHeaderRows)) | isRowHeader headerRow = pure $
    simpleTable (rowToBlocks headerRow) $ map rowToBlocks nonHeaderRows
                                          | otherwise = pure $
    verySimpleTable $ map rowToBlocks rows
itemToBlocks level (UserDef type_ content) = case getReader type_ of
    Just reader -> nested <$> reader def (unlines content)
    Nothing -> itemToBlocks level $ Body (linesToParagraphs content)
itemToBlocks _ (PreUserDef (Just "IMAGE") content) = pure $ P.plain $ P.image url ("title is " ++ url) (P.text $ "alt is " ++ url)
  where url = takeUntilFirst '\n' content
itemToBlocks _ (PreUserDef type_ content) = pure $ P.codeBlockWith ("", maybeToList type_, []) content


getReader :: Maybe String -> Maybe (P.ReaderOptions -> String -> IO Pandoc)
getReader type_ = do
    format <- map toLower <$> type_
    lookup format P.readers

nested :: Pandoc -> P.Blocks
nested (P.Pandoc _ blocks) = P.fromList blocks


takeUntilFirst :: Eq a => a -> [a] -> [a]
takeUntilFirst c = takeWhile (/= c)


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
        emptyCaption = mempty
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
