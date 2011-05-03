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
  , ItemContent(..)
  , TableRow(..)
  , getOutlineTitleItem
  , getOutlineNonTitleItems
  , getItemTitle
) where



import Text.OTL.Parser
import Text.OTL.Types
