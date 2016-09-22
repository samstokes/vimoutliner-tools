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
  , TableRow(..)
  , getOutlineTitleItem
  , getOutlineNonTitleItems
  , splitBy
  , linesToParagraphs
) where



newtype Outline = Outline { getOutlineItems :: [Item] }
  deriving (Eq, Show)

data Item = Heading { getHeading :: String
                    , getHeadingChildren :: [Item]
                    }
            | Body { getBodyParagraphs :: [String] }
            | Preformatted { getPreformattedContent :: String }
            | Table { getTableRows :: [TableRow] }
            | UserDef { getUserDefType :: Maybe String
                      , getUserDefLines :: [String]
                      }
            | PreUserDef { getUserDefType :: Maybe String
                         , getPreformattedContent :: String
                         }
  deriving (Eq, Show)

data TableRow = TableRow { isRowHeader :: Bool
                         , getRowEntries :: [String]
                         }
  deriving (Eq, Show)



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
  segregateTitle' (Outline (title@(Heading _ _) : nonTitles)) = Right (title, nonTitles)
  segregateTitle' (Outline (item : _)) = Left $ "unexpected non-heading as first item: " ++ show item
  segregateTitle' _ = Left "empty outline!"


linesToParagraphs :: [String] -> [String]
linesToParagraphs = unlinesSplitByBlanks

unlinesSplitByBlanks :: [String] -> [String]
unlinesSplitByBlanks = map unlines . splitBy null

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = foldr addUnlessP []
    where
    addUnlessP item groups | p item = [] : groups
    addUnlessP item [] = [[item]]
    addUnlessP item (group : groups) = (item : group) : groups
