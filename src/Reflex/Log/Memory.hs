{-# LANGUAGE FlexibleContexts #-}
module Reflex.Log.Memory (
    mkMemoryLog
  , module Reflex.Log
  ) where

import Control.Monad.Fix (MonadFix)

import Control.Monad.Trans (MonadIO, liftIO)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.STM

import Reflex

import Reflex.Log
import Reflex.Log.Backend

data LogState e s =
  LogState {
    _lsNextEventId :: TVar EventId
  , _lsEvents :: TVar (Map EventId e)
  , _lsSnapshots :: TVar (Map EventId s)
  }

newLogState :: STM (LogState e s)
newLogState =
  LogState <$>
    newTVar (EventId 1) <*>
    newTVar Map.empty <*>
    newTVar Map.empty

loadFromLog :: LogState e s
            -> (e -> s -> s)
            -> EventId
            -> s
            -> STM (Int, (EventId, s))
loadFromLog st f ii is = do
  size <- fmap Map.size . readTVar $ _lsEvents st

  -- load the snapshot with the highest id
  -- - use the snapshot id and value to start the fold, otherwise default to the initial values provided
  snapshotMap <- readTVar $ _lsSnapshots st
  let
    (zi, zs) = case Map.maxViewWithKey snapshotMap of
      Nothing -> (ii, is)
      Just ((k, v), _) -> (k, v)

  -- load all of the events after that id
  eventsMap <- readTVar $ _lsEvents st
  let
    es = Map.toList . Map.filterWithKey (\k _ -> k > zi) $  eventsMap

  let
    res = foldr (\(i, e) (_, s) -> (i, f e s)) (zi, zs) es

  writeTVar (_lsNextEventId st) ((\(EventId i) -> EventId (i + 1)) . fst $ res)

  pure (size, res)

logEvent :: LogState e s
         -> e
         -> STM (EventId, e)
logEvent st e = do
  i <- readTVar (_lsNextEventId st)
  modifyTVar (_lsNextEventId st) (\(EventId x) -> EventId (x + 1))
  modifyTVar (_lsEvents st) (Map.insert i e)
  pure (i, e)

createSnapshot :: LogState e s
               -> (EventId, s)
               -> STM ()
createSnapshot st (i, s) = do
  modifyTVar (_lsSnapshots st) (Map.insert i s)

vacuumLog :: LogState e s
          -> STM ()
vacuumLog st = do
  mV <- fmap Map.maxViewWithKey . readTVar $ _lsSnapshots st
  case mV of
    Nothing -> pure ()
    Just ((s, _), _) -> do
      modifyTVar (_lsSnapshots st) . Map.filterWithKey $ \k _ -> k >= s
      modifyTVar (_lsEvents st) . Map.filterWithKey $ \k _ -> k > s

memoryBackend :: LogState e s -> ReflexLogBackend e s
memoryBackend st =
  ReflexLogBackend
    (\f ii is -> atomically $ loadFromLog st f ii is)
    (atomically . logEvent st)
    (atomically . createSnapshot st)
    (atomically . vacuumLog $ st)

mkMemoryLog :: (MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadIO m)
            => LogConfig t e s
            -> m (Log t s)
mkMemoryLog lc = do
  st <- liftIO . atomically $ newLogState
  mkLog (memoryBackend st) lc
