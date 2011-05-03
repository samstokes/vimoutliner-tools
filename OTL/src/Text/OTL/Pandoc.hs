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


toPandoc :: Outline -> Pandoc
toPandoc outline = P.setTitle outlineTitle $ P.doc $
    foldMap (itemToBlocks 1) (titleChildren ++ nonTitleItems)
    where
      titleItem = getOutlineTitleItem outline
      outlineTitle = P.text $ getHeading titleItem
      titleChildren = getHeadingChildren titleItem
      nonTitleItems = getOutlineNonTitleItems outline


defaultWriterOptions :: IO P.WriterOptions
defaultWriterOptions = do
  templateFile <- PS.readDataFile Nothing "templates/html.template"
  return P.defaultWriterOptions {
      P.writerStandalone = True
    , P.writerTemplate = templateFile
    }


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
itemToBlocks _ (Table rows) = P.simpleTable [] $ map (map (P.plain . P.text) . getRowEntries) rows
itemToBlocks level (UserDef _ content) = itemToBlocks level $ Body [content]
itemToBlocks level (PreUserDef _ content) = itemToBlocks level $ Preformatted content
