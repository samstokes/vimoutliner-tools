-----------------------------------------------------------------------------
--
-- Module      :  Text.OTL.Types
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

module Text.OTL.Types (
    Outline(..)
  , Item(..)
  , ItemContent(..)
  , TableRow(..)
) where



newtype Outline = Outline { getOutlineItems :: [Item] }
  deriving (Show)

data Item = Item { getItemContent :: ItemContent
                 , getItemChildren :: [Item]
                 }
  deriving (Show)

data ItemContent = Heading { getHeading :: String }
                 | Body { getBodyParagraphs :: [String] }
                 | Preformatted { getPreformattedContent :: String }
                 | Table { getTableRows :: [TableRow] }
                 | UserDef { getUserDefType :: Maybe String
                           , getUserDefContent :: String
                           }
                 | PreUserDef { getUserDefType :: Maybe String
                              , getPreformattedContent :: String
                              }
  deriving (Show)

data TableRow = TableRow { isRowHeader :: Bool
                         , getRowEntries :: [String]
                         }
  deriving (Show)
