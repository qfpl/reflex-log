{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Log.SQLite (
    mkSQLiteLog
  ) where

import Data.Monoid
import Data.Proxy (Proxy(..))

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
  , _lsEventIdTable :: Query
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

loadFromLog :: forall e s.
               (Binary e, Binary s)
            => LogState
            -> (e -> s -> s)
            -> EventId
            -> s
            -> IO (Int, (EventId, s))
loadFromLog (LogState conn _ eventTable snapshotTable) f ii is =
  withTransaction conn $ do
    counts <- query_ conn $ "SELECT COUNT(id) FROM " <> eventTable <> " GROUP BY id"
    let
      count = case counts of
        [] -> 0
        (Only c : _) -> c
    snapshots <- query_ conn $ "SELECT * FROM " <> snapshotTable <> " ORDER BY id DESC LIMIT 1" :: IO [SnapshotField s]
    let
      (zi, zs) :: (EventId, s) = case snapshots of
        [] -> (ii, is)
        (SnapshotField i s : _) -> (i, s)

    es' <- query conn ("SELECT * FROM " <> eventTable <> " WHERE id > ?") (Only zi)

    let
      es = fmap (\(EventField i e) -> (i, e)) es'
      res = foldr (\(i, e) (_, s) -> (i, f e s)) (zi, zs) es

    pure (count, res)

logEvent :: Binary e
         => LogState
         -> e
         -> IO (EventId, e)
logEvent (LogState conn eventIdTable eventTable _) e =
  withTransaction conn $ do
    [Only i] <- query_ conn $ "SELECT * FROM " <> eventIdTable
    execute conn ("INSERT INTO " <> eventTable <> " VALUES (?, ?)") (EventField i e)
    execute_ conn $ "UPDATE " <> eventIdTable <> " SET id = id + 1"
    pure $ (i, e)

createSnapshot :: Binary s
               => LogState
               -> (EventId, s)
               -> IO ()
createSnapshot (LogState conn _ _ snapshotTable) (i, s) =
  withTransaction conn $ do
    execute conn ("INSERT INTO " <> snapshotTable <> " VALUES (?,?)") (SnapshotField i s)

vacuumLog :: forall s.
             Binary s
          => Proxy s
          -> LogState
          -> IO ()
vacuumLog _ (LogState conn _ eventTable snapshotTable) = do
  withTransaction conn $ do
    is <- query_ conn $ "SELECT * FROM " <> snapshotTable <> " ORDER BY id DESC LIMIT 1" :: IO [SnapshotField s]
    case is of
      [] -> pure ()
      (SnapshotField i _ : _) -> do
        execute conn ("DELETE FROM " <> snapshotTable <> " WHERE id < ?") (Only $ i)
        execute conn ("DELETE FROM " <> eventTable <> " WHERE id <= ?") (Only $ i)
  execute_ conn "VACUUM"

sqliteBackend :: forall e s.
                 (Binary e, Binary s)
              => LogState
              -> ReflexLogBackend e s
sqliteBackend st =
  ReflexLogBackend
    (loadFromLog st)
    (logEvent st)
    (createSnapshot st)
    (vacuumLog (Proxy :: Proxy s) st)

mkSQLiteLog :: (MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadIO m, Binary e, Binary s)
          => Connection
          -> Query
          -> LogConfig t e s
          -> m (Log t s)
mkSQLiteLog conn baseName lc = do
  let
    eventIdTable = baseName <> "_event_id"
    eventTable = baseName <> "_events"
    snapshotTable = baseName <> "_snapshots"
    st = LogState conn eventIdTable eventTable snapshotTable

  liftIO . withTransaction conn $ do
    execute_ conn $
      "CREATE TABLE IF NOT EXISTS " <> eventIdTable <> " (id INTEGER PRIMARY KEY)"

    ids <- query_ conn $ "SELECT * FROM " <> eventIdTable :: IO [Only Int]
    case ids of
      [] -> execute_ conn $ "INSERT INTO " <> eventIdTable <> " VALUES (1)"
      _ -> pure ()

    execute_ conn $
      "CREATE TABLE IF NOT EXISTS " <> eventTable <> " (id INTEGER PRIMARY KEY, event BLOB)"
    execute_ conn $
      "CREATE TABLE IF NOT EXISTS " <> snapshotTable <> " (id INTEGER PRIMARY KEY, snapshot BLOB)"

  mkLog (sqliteBackend st) lc

