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
toPandoc outline = P.setTitle outlineTitle $ P.doc $ foldMap itemToBlocks outlineItems
    where outlineTitle = P.text $ getHeading (getOutlineTitleItem outline)
          outlineItems = getOutlineItems outline


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
