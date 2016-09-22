-----------------------------------------------------------------------------
--
-- Module      :  Text.OTL
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

module Text.OTL (
    parser
  , parse
  , Outline(..)
  , Item(..)
  , TableRow(..)
  , getOutlineTitleItem
  , getOutlineNonTitleItems
) where



import Text.OTL.Parser
import Text.OTL.Types
