{-# LANGUAGE TemplateHaskell #-}
module Reflex.Log (
    LogConfig(..)
  , lcFunction
  , lcInitialValue
  , lcLog
  , lcSnapshot
  , lcVacuum
  , Log(..)
  , lValue
  , lSize
  , lSnapshotComplete
  , lVacuumComplete
  ) where

import Control.Lens

import Reflex

data LogConfig t e s =
  LogConfig {
    _lcFunction     :: e -> s -> s
  , _lcInitialValue :: s
  , _lcLog          :: Event t e
  , _lcSnapshot     :: Event t ()
  , _lcVacuum       :: Event t ()
  }

makeLenses ''LogConfig

data Log t s =
  Log {
    _lValue :: Dynamic t s
  , _lSize  :: Dynamic t Int
  , _lSnapshotComplete :: Event t ()
  , _lVacuumComplete :: Event t ()
  }

makeLenses ''Log
