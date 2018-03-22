{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Log.File (
    mkFileLog
  ) where

import Control.Monad (forM, forM_, when)

import Control.Monad.Fix (MonadFix)

import Control.Monad.Trans (MonadIO, liftIO)

import Data.Map (Map)
import qualified Data.Map as Map

import System.FilePath
import System.Directory

import Reflex

import Reflex.Log
import Reflex.Log.Backend

data LogState =
  LogState {
    _lsEventLog :: FilePath
  , _lsSnapshotLog :: FilePath
  }

loadIds :: Read k => FilePath -> IO [k]
loadIds base = do
  files <- listDirectory base
  pure $ fmap read files

highestId :: (Ord k, Read k) => FilePath -> IO (Maybe k)
highestId base = do
  ids <- loadIds base
  pure $ case ids of
    [] -> Nothing
    xs -> Just . maximum $ xs

loadMap :: (Ord k, Read k, Read v) => FilePath -> IO (Map k v)
loadMap base = do
  fps <- listDirectory base
  ls <- forM fps $ \fp -> do
    contents <- readFile $ base </> fp
    pure (read . takeFileName $ fp, read contents)
  pure $ Map.fromList ls

removeLower :: (Ord k, Read k) => k -> FilePath -> IO ()
removeLower k base = do
  contents <- listDirectory base
  forM_ contents $ \fp ->
    when (read fp < k) $
      removeFile $ base </> fp

loadFromLog :: (Read e, Read s)
            => LogState
            -> (e -> s -> s)
            -> EventId
            -> s
            -> IO (Int, (EventId, s))
loadFromLog st f ii is = do

  snapshotMap <- loadMap $ _lsSnapshotLog st
  let
    (zi, zs) = case Map.maxViewWithKey snapshotMap of
      Nothing -> (ii, is)
      Just ((k, v), _) -> (k, v)

  eventsMap <- loadMap $ _lsEventLog st
  let
    es = Map.toList . Map.filterWithKey (\k _ -> k > zi) $ eventsMap

  let
    res = foldr (\(i, e) (_, s) -> (i, f e s)) (zi, zs) es

  ids :: [EventId] <- loadIds $ _lsEventLog st

  pure (length ids, res)

logEvent :: Show e
         => LogState
         -> e
         -> IO (EventId, e)
logEvent st e = do
  let
    base = _lsEventLog st
  eid <- highestId base
  sid <- highestId $ _lsSnapshotLog st
  let
    newKey = case eid of
      Nothing -> case sid of
        Nothing -> EventId 1
        Just (EventId x) -> EventId (x + 1)
      Just (EventId x) -> EventId (x + 1)
  writeFile (base </> show newKey) (show e)
  pure (newKey, e)

createSnapshot :: Show s
               => LogState
               -> (EventId, s)
               -> IO ()
createSnapshot st (i, s) = do
  let
    base = _lsSnapshotLog st
  writeFile (base </> show i) (show s)

vacuumLog :: LogState
          -> IO ()
vacuumLog st = do
  msid <- highestId $ _lsSnapshotLog st
  case msid of
    Nothing -> pure ()
    Just sid@(EventId i) -> do
      removeLower (EventId (i + 1)) $ _lsEventLog st
      removeLower sid $ _lsSnapshotLog st

fileBackend :: (Read e, Show e, Read s, Show s)
            => LogState
            -> ReflexLogBackend e s
fileBackend st =
  ReflexLogBackend
    (loadFromLog st)
    (logEvent st)
    (createSnapshot st)
    (vacuumLog st)

mkFileLog :: (MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadIO m, Read e, Show e, Show s, Read s)
          => FilePath
          -> LogConfig t e s
          -> m (Log t s)
mkFileLog baseName lc = do
  let
    st = LogState ("." </> baseName </> "event") ("." </> baseName </> "snapshot")
  liftIO $ do
    createDirectoryIfMissing True $ _lsEventLog st
    createDirectoryIfMissing True $ _lsSnapshotLog st
  mkLog (fileBackend st) lc
