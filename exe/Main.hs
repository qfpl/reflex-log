{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (
    main
  ) where

import Control.Monad (void)

import Reflex.Dom

import Reflex.Log
import Reflex.Log.File

main :: IO ()
main = mainWidget testMe

-- A set of commands that we would like to process:
data Cmd =
    Add Int
  | Total
  | Clear
  deriving (Eq, Ord, Read, Show)

-- The processing that we want to do:
calc :: Cmd -> Int -> Int
calc (Add x) i = x + i
calc Total i = i
calc Clear _ = 0

testMe :: MonadWidget t m => m ()
testMe = el "div" $ mdo
  -- Events which fire when we manually trigger a snapshot or a vacuum
  (eSnapshotButton, eVacuumButton) <- el "div" $
    (,) <$> button "Snapshot" <*> button "Vacuum"

  -- An event which fires with the command that we have selected
  eCmd <- el "div" $ do
    eAdd1  <- button "Add 1"
    eAdd5  <- button "Add 5"
    eTotal <- button "Total"
    eClear <- button "Clear"
    pure $ leftmost [Add 1 <$ eAdd1, Add 5 <$ eAdd5, Total <$ eTotal, Clear <$ eClear]

  let
    -- snapshot the events whenever the user presses the snapshot button, or
    -- if there are more than 10 events in the log
    eSnapshot = leftmost [eSnapshotButton, void . ffilter (10 <=) . updated . _lSize $ log]
    -- vacuum the log whenever the user presses the vacuum button
    eVacuum = eVacuumButton
    -- usually we would vacuum the log as soon as the snapshotting was complete
    -- eVacuum = _lSnapshotComplete log

  -- Create a log of the events on the file system
  log <- mkFileLog "testme" $ LogConfig calc 0 eCmd eSnapshot eVacuum
  let dValue = _lValue log

  -- This is another way we could do things like this, see the comment block in Reflex.Log.Backend for details
  -- let lc = LogConfig "testme" eSnapshot eVacuum
  -- (dValue, log) <- foldFileLog lc calc o eCmd

  -- Display the accumulated value of processing the events
  el "div" $
    display $ dValue

  pure ()
