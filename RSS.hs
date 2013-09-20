{-# LANGUAGE Arrows #-}
module RSS where

import Prelude hiding ((.), id)
import Control.Category

import Download

import Data.Text (Text, pack)
import Text.XML.HXT.Core

data FeedEntry = FeedEntry
    { entryTitle :: Text
    , entryDescription :: Text
    , entryLink :: String
    }
    deriving Show

getFeeds :: ArrowXml a => a XmlTree FeedEntry
getFeeds = proc x -> do
    item <- deep (hasName "item") -< x
    title <- getChildren >>> hasName "title" /> getText -< item
    descr <- getChildren >>> hasName "description" /> getText -< item
    link <- getChildren >>> hasName "link" /> getText -< item
    returnA -< FeedEntry (pack title) (pack descr) link

readRSS :: String -> IO [FeedEntry]
readRSS url = do
    doc <- get url
    runX $ doc >>> getFeeds


