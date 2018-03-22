{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Log.Backend (
    EventId(..)
  , SnapshotId(..)
  , ReflexLogBackend(..)
  , mkLog
  , module Reflex.Log
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadIO, liftIO)

import Reflex

import Reflex.Log

newtype EventId = EventId { getEventId :: Int }
  deriving (Eq, Ord)

instance Show EventId where
  showsPrec n (EventId i) = showsPrec n i

instance Read EventId where
  readsPrec n = fmap (\(x,s) -> (EventId x, s)) . readsPrec n

newtype SnapshotId = SnapshotId { getSnapshotId :: Int }
  deriving (Eq, Ord)

instance Show SnapshotId where
  showsPrec n (SnapshotId i) = showsPrec n i

instance Read SnapshotId where
  readsPrec n = fmap (\(x,s) -> (SnapshotId x, s)) . readsPrec n

data ReflexLogBackend e s =
  ReflexLogBackend {
    loadFromLog' :: (e -> s -> s) -> EventId -> s -> IO (Int, (EventId, s))
  , logEvent' :: e -> IO (EventId, e)
  , createSnapshot' :: (EventId, s) -> IO ()
  , vacuumLog' :: IO ()
  }

loadFromLog :: (PerformEvent t m, MonadIO (Performable m))
            => ReflexLogBackend e s
            -> (e -> s -> s)
            -> EventId
            -> s
            -> Event t ()
            -> m (Event t (Int, (EventId, s)))
loadFromLog be f ii is eLoad =
  performEvent $ (liftIO $ loadFromLog' be f ii is) <$ eLoad

logEvent :: (PerformEvent t m, MonadIO (Performable m))
         => ReflexLogBackend e s
         -> Event t e
         -> m (Event t (EventId, e))
logEvent be eLog =
  performEvent $ liftIO . logEvent' be <$> eLog

createSnapshot :: (PerformEvent t m, MonadIO (Performable m))
               => ReflexLogBackend e s
               -> Event t (EventId, s)
               -> m (Event t ())
createSnapshot be eSnapshot =
  performEvent $ liftIO . createSnapshot' be <$> eSnapshot

vacuumLog :: (PerformEvent t m, MonadIO (Performable m))
          => ReflexLogBackend e s
          -> Event t ()
          -> m (Event t ())
vacuumLog be eVacuum =
  performEvent $ liftIO (vacuumLog' be) <$ eVacuum

{-
This is an alternative approach, that looks more like foldDyn:

foldLog :: (Reflex t, MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m))
        => ReflexLogBackend e s
        -> LogConfig t
        -> (e -> s -> s)
        -> s
        -> Event t e
        -> m (Dynamic t s, Log t)

That would be more suitable for inline usage, where the current mkLog could probably be altered to use
EventWriter t (NonEmpty e) and ReaderT (Dynamic t s) so that you can have one log in an application, with
multiple parts of the application logging events and/or querying the accumulated state.
-}

mkLog :: (Reflex t, MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m))
      => ReflexLogBackend e s
      -> LogConfig t e s
      -> m (Log t s)
mkLog be (LogConfig f i eLog eSnapshot eVacuum) = do
  ePostBuild <- getPostBuild

  eLoad <- loadFromLog be f (EventId 0) i ePostBuild
  eLog' <- logEvent be eLog

  dLog' <- foldDyn ($) (EventId 0, i) . mergeWith (.) $ [
             const . snd <$> eLoad
           , (\(eid, e) (_, s) -> (eid, f e s)) <$> eLog'
           ]

  eSnapshotComplete <- createSnapshot be $ current dLog' <@ eSnapshot
  eVacuumComplete <- vacuumLog be eVacuum

  let
    dLog = snd <$> dLog'

  dSize <- foldDyn ($) 0 . mergeWith (.) $ [
             const . fst <$> eLoad
           , const 0     <$ eVacuum
           , (+ 1)       <$ eLog
           ]

  pure $ Log dLog dSize eSnapshotComplete eVacuumComplete
