{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleContexts,
    GADTs, OverloadedStrings #-}
module Db where

import Data.Text (Text)
import Data.Time.Clock

import Database.Persist
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Article
    uri String
    description Text
    timestamp UTCTime
    UniqueUri uri
    deriving Show

Keyword
    word Text
    UniqueKeyword word
    deriving Show

WordRel
    word KeywordId
    article ArticleId
    occurences Int
    timestamp UTCTime
    deriving Show
|]

getOrInsert :: (PersistUnique m, PersistEntity val,
                PersistMonadBackend m ~ PersistEntityBackend val)
            => val -> m (Key val)
getOrInsert val = do
    res <- insertBy val
    return $ case res of
        Left entity -> entityKey entity
        Right key -> key

