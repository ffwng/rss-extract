{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections #-}
module Main where

import Pipes
import Database.Persist
import Database.Persist.Sqlite

import Data.Text (pack)
import Data.Time.Clock
import Data.Map (Map, fromListWith, toList)

import Control.Monad

import Text.XML.HXT.Core

import Options.Applicative

import Db
import Download
import RSS

processFeed :: (PersistUnique m, PersistMonadBackend m ~ SqlBackend) => FeedEntry -> m ()
processFeed entry = do
    timestamp <- liftIO getCurrentTime
    let uri = entryLink entry
    liftIO $ putStrLn $ "Processing " ++ uri
    let descr = entryDescription entry
    let art = Article uri descr timestamp
    res <- insertUnique art
    case res of
        Nothing -> return ()
        Just key -> processWords uri key timestamp

processWords :: (PersistUnique m, PersistMonadBackend m ~ SqlBackend)
             => String -> ArticleId -> UTCTime -> m ()
processWords uri artId timestamp = do
    doc <- liftIO $ getHTML uri
    texts <- liftIO . runX $ doc >>> deep getText
    let go (word, count) = do
        wordId <- getOrInsert $ Keyword (pack word)
        insert $ WordRel wordId artId count timestamp
    let wordMap = fromListWith (+) . map (,1) $ concatMap words texts
    mapM_ go $ toList wordMap

data Args = Args
    { inputFile :: FilePath
    , outputFile :: FilePath
    }

args :: Parser Args
args = Args
    <$> strOption
        ( long "uri-file"
        <> short 'i'
        <> metavar "URI-FILE"
        <> help "File with URIs"
        )
    <*> strOption
        ( long "db-file"
        <> short 'o'
        <> metavar "DB-FILE"
        <> help "Output database file"
        )

main :: IO ()
main = do
    Args input output <- execParser $ info (helper <*> args) fullDesc
    uris <- lines <$> readFile input
    runSqlite (pack output) $ do
        runMigration migrateAll
        forM_ uris $ \uri -> do
            feeds <- liftIO $ readRSS uri
            mapM_ processFeed feeds
