{-# LANGUAGE OverloadedStrings #-}

module Utils.WebScraping (
    Text,
    queryParamsFromURI,
    loadHTML,
    getTextAsText,
    elementsWithAttributeValue,
    divsWithID,
    divsWithClass,
    asidesWithClass,
    spansWithClass,
    tablesWithClass,
    tableCells,
    getLinkTargets,
    getLinkTargetsAndText,
    getImageSources,
    getImageSourcesWithClass,
    packAll,
    makePairsFromList,
    findRoadNameInText,
    textAfter
) where

import           Control.Monad       (join)
import           Data.Char
import qualified Data.Text      as T
import qualified Debug.Trace         (trace)
import qualified Text.URI       as URI
import           Text.XML.HXT.Core
import           Text.XML.HXT.Curl

type Text = T.Text

-- Split the query part of a URI into an associative array of key-value pairs
queryParamsFromURI :: Text -> Maybe [(Text,Text)]
queryParamsFromURI uri = fmap (map toTextPair) stringPairs
    where
        queryString = join $ fmap URI.uriQuery $ URI.parseURI $ T.unpack uri
        stringPairs = fmap URI.queryToPairs queryString
        toTextPair (x,y) = (T.pack x, T.pack y)


-- Load an HTML DOM from an absolute or relative URL. If the URL is relative,
-- it is prefixed with the base URL.
loadHTML :: Text -> IOStateArrow s b XmlTree
loadHTML url = (Debug.Trace.trace $ "GET " ++ T.unpack url) $
     readDocument [withParseHTML         yes,
                   withWarnings          no,
                   withStrictInput       yes,
                   withStrictDeserialize yes,
                   withValidate          no,
                   withPreserveComment   yes,
                   withCurl              curlOpts] (T.unpack url)
    where
        curlOpts = [("-A", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")]

-- Utility HXT filter to getText from nodes and pack to Data.Text
getTextAsText :: ArrowXml a => a XmlTree Text
getTextAsText = getText >>. packAll

-- HXT filter to find a specific element type with a matching attribute vlaue
elementsWithAttributeValue :: ArrowXml a => String -> String -> String -> a XmlTree XmlTree
elementsWithAttributeValue elementName attributeName attributeValue =
    deep (isElem
          >>> hasName elementName
          >>> hasAttrValue attributeName (== attributeValue))

-- HXT filter to find divs by id
divsWithID :: ArrowXml a => String -> a XmlTree XmlTree
divsWithID = elementsWithAttributeValue "div" "id"

-- HXT filter to find divs by class
divsWithClass :: ArrowXml a => String -> a XmlTree XmlTree
divsWithClass = elementsWithAttributeValue "div" "class"

-- HXT filter to find asides by class
asidesWithClass :: ArrowXml a => String -> a XmlTree XmlTree
asidesWithClass = elementsWithAttributeValue "aside" "class"

-- HXT filter to find spans by class
spansWithClass :: ArrowXml a => String -> a XmlTree XmlTree
spansWithClass = elementsWithAttributeValue "span" "class"

-- HXT filter to find tables with a css class
tablesWithClass :: ArrowXml a => String -> a XmlTree XmlTree
tablesWithClass = elementsWithAttributeValue "table" "class"

-- HXT filter to find table cells
tableCells :: ArrowXml a => a XmlTree XmlTree
tableCells = deep (isElem >>> hasName "td")

-- HXT filter to get <A> href targets
getLinkTargets :: ArrowXml a => a XmlTree Text
getLinkTargets = hasName "a" >>> getAttrValue "href" >>. packAll

-- HXT filter to get <A> href targets and the accompanying text
getLinkTargetsAndText :: ArrowXml a => a XmlTree (Text, Text)
getLinkTargetsAndText = hasName "a" >>> (links <+> text) >>. mkPairs
    where
        links = getAttrValue "href" >>. packAll
        text = getChildren >>> getTextAsText
        mkPairs [] = []
        mkPairs (x:y:ys) = (x,y):mkPairs ys

-- HXT filter to get image sources
getImageSources :: ArrowXml a => a XmlTree Text
getImageSources = hasName "img" >>> getAttrValue "src" >>. packAll

-- HXT filter to get image sources for images with a class name
getImageSourcesWithClass :: ArrowXml a => String -> a XmlTree Text
getImageSourcesWithClass cls = deep (isElem
                                     >>> hasName "img"
                                     >>> hasAttrValue "class" (== cls)
                                     >>> getAttrValue "src"
                                     >>. packAll)

packAll :: [String] -> [Text]
packAll = map T.pack

makePairsFromList :: [a] -> [(a,a)]
makePairsFromList (x:y:ys) = (x,y):makePairsFromList ys
makePairsFromList _ = []

findRoadNameInText :: Text -> Text
findRoadNameInText text = if null wordsLikeRoadNames then "" else head wordsLikeRoadNames
    where
        wordsLikeRoadNames = filter likeRoadName $ T.words text
        likeRoadName word = (T.head word) `elem` ['A','B','M']
                            && T.all isDigit (T.tail word)

textAfter :: Text -> Text -> Text
textAfter _ "" = ""
textAfter "" text = text 
textAfter marker text = T.strip . snd $ T.breakOnEnd marker text
