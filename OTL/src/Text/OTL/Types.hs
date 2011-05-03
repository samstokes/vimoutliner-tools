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
  , getOutlineTitleItem
  , getOutlineNonTitleItems
  , getItemTitle
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



{- UTILITIES -}


getOutlineTitleItem :: Outline -> Item
getOutlineTitleItem = fst . segregateTitle


getOutlineNonTitleItems :: Outline -> [Item]
getOutlineNonTitleItems = snd . segregateTitle


segregateTitle :: Outline -> (Item, [Item])
segregateTitle outline = case segregateTitle' outline of
    Right items -> items
    Left reason -> error $ "Couldn't find title item: " ++ reason
  where
  segregateTitle' (Outline (title@(Item (Heading _) _) : nonTitles)) = Right (title, nonTitles)
  segregateTitle' (Outline (item : _)) = Left $ "unexpected non-heading as first item: " ++ show item
  segregateTitle' _ = Left "empty outline!"


getItemTitle :: Item -> String
getItemTitle (Item (Heading heading) _) = heading
getItemTitle item = error $ "Couldn't get title for non-heading: " ++ show item
