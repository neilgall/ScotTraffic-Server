{-# LANGUAGE OverloadedStrings #-}

module Utils.RoadName (splitRoadNameFromTitle) where

import           Data.Bool (bool)
import           Data.Char
import qualified Data.Text as T

-- Extract a road name from a camera page title if possible. If the title begins
-- with the pattern [ABM][0-9/()]+ (e.g. A90, A823/4, M8, B8023) this is removed
-- as the road name. If the first remaining word is "at" this is also removed.
-- The remainder of the string is cleaned up and returned as the new title. If no
-- road name can be matched, an empty road name and the original title is returned.
splitRoadNameFromTitle :: T.Text -> (Maybe T.Text, Maybe T.Text)
splitRoadNameFromTitle "" = (Nothing, Nothing)
splitRoadNameFromTitle title = (Just road, Just $ cleanUpName road name)
    where
        w = T.words title
        fw = head w
        tw = T.unwords $ tail w
        (road, name) = if isRoadName fw then (fw, tw) else ("", title)

-- Clean up unwanted text in the title string
cleanUpName :: T.Text -> T.Text -> T.Text
cleanUpName road = 
    T.unwords . map unparenRoadName . removeRoadName road . removeAtPrefix . T.words

removeRoadName :: T.Text -> [T.Text] -> [T.Text]
removeRoadName road
    | T.null road = id
    | otherwise = remove road . remove parenthesisedRoad . replace dashPrefixedRoad "to"
    where
        remove t = filter (/= t)
        replace t t' = map (\w -> bool w t' (w == t))
        parenthesisedRoad = T.cons '(' . flip T.snoc ')' $ road
        dashPrefixedRoad = T.cons '-' road

removeAtPrefix :: [T.Text] -> [T.Text]
removeAtPrefix [] = []
removeAtPrefix (t:ts)
    | t == "at" = ts
    | otherwise = t:ts
    
unparenRoadName :: T.Text -> T.Text
unparenRoadName t
    | T.null t = t
    | and [T.head t == '(', T.last t == ')', isRoadName unparen] = unparen
    | otherwise = t
    where 
        unparen = T.tail . T.init $ t

-- Is a string a road name? e.g. A90, B843, M74, A74(M)/M74
isRoadName :: T.Text -> Bool
isRoadName t = isLeadingChar (T.head t) && T.all isTrailingChar (T.tail t)
    where
        isLeadingChar c = c `elem` ['A','B','M']
        isTrailingChar c = isLeadingChar c || isDigit c || c `elem` ['/', '(', ')']
