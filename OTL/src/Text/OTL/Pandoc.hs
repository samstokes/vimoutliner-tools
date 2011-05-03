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
toPandoc outline = P.setTitle outlineTitle $ P.doc $ P.orderedList (map itemToBlocks (titleChildren ++ nonTitleItems))
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


itemToBlocks :: Item -> P.Blocks
itemToBlocks (Heading heading children) =
    (P.plain . P.text) heading `mappend`
    P.orderedList (map itemToBlocks children)
