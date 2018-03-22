{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Log.SQLite (
    mkSQLiteLog
  ) where

import Data.Monoid

import Control.Monad.Fix (MonadFix)

import Control.Monad.Trans (MonadIO, liftIO)

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import Data.Binary
-- import qualified Data.ByteString.Lazy as LBS

import Reflex hiding (Query)

import Reflex.Log
import Reflex.Log.Backend

data LogState =
  LogState {
    _lsConn :: Connection
  , _lsEventTable :: Query
  , _lsSnapshotTable :: Query
  }

instance FromField EventId where
  fromField = fmap (fmap EventId) fromField

instance ToField EventId where
  toField (EventId i) = toField i

data EventField e =
  EventField {
    _efId :: EventId
  , _efEvent :: e
  } deriving (Eq, Ord, Show)

instance Binary e => FromRow (EventField e) where
  fromRow = EventField <$> field <*> (fmap decode field)

instance Binary e => ToRow (EventField e) where
  toRow (EventField eid e) = toRow (eid, encode e)

data SnapshotField s =
  SnapshotField {
    _sfId :: EventId
  , _sfSnapshot :: s
  } deriving (Eq, Ord, Show)

instance Binary s => FromRow (SnapshotField s) where
  fromRow = SnapshotField <$> field <*> (fmap decode field)

instance Binary s => ToRow (SnapshotField s) where
  toRow (SnapshotField sid s) = toRow (sid, encode s)

loadFromLog :: (Binary e, Binary s)
            => LogState
            -> (e -> s -> s)
            -> EventId
            -> s
            -> IO (Int, (EventId, s))
loadFromLog st f ii is = undefined

logEvent :: Binary e
         => LogState
         -> e
         -> IO (EventId, e)
logEvent st e = undefined

createSnapshot :: Binary s
               => LogState
               -> (EventId, s)
               -> IO ()
createSnapshot st (i, s) =
  execute (_lsConn st) ("INSERT INTO " <> _lsSnapshotTable st <> " VALUES (?,?)") (SnapshotField i s)

vacuumLog :: LogState
          -> IO ()
vacuumLog st = undefined

sqliteBackend :: (Binary e, Binary s)
              => LogState
              -> ReflexLogBackend e s
sqliteBackend st =
  ReflexLogBackend
    (loadFromLog st)
    (logEvent st)
    (createSnapshot st)
    (vacuumLog st)

mkSQLiteLog :: (MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadIO m, Binary e, Binary s)
          => Connection
          -> Query
          -> LogConfig t e s
          -> m (Log t s)
mkSQLiteLog conn baseName lc = do
  let st = LogState conn (baseName <> "_events") (baseName <> "_snapshots")
  liftIO $ do
    execute_ conn $
      "CREATE TABLE IF NOT EXISTS " <> _lsEventTable st <> " (id INTEGER PRIMARY KEY, event BLOB)"
    execute_ conn $
      "CREATE TABLE IF NOT EXISTS " <> _lsSnapshotTable st <> " (id INTEGER PRIMARY KEY, snapshot BLOB)"
  mkLog (sqliteBackend st) lc

