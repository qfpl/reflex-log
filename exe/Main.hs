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

data Cmd = Add Int | Total | Clear
  deriving (Eq, Ord, Read, Show)

calc :: Cmd -> Int -> Int
calc (Add x) i = x + i
calc Total i = i
calc Clear _ = 0

testMe :: MonadWidget t m => m ()
testMe = el "div" $ mdo
  (eSnapshot', eVacuum') <- el "div" $
    (,) <$> button "Snapshot" <*> button "Vacuum"

  eCmd <- el "div" $ do
    eAdd1  <- button "Add 1"
    eAdd5  <- button "Add 5"
    eTotal <- button "Total"
    eClear <- button "Clear"
    pure $ leftmost [Add 1 <$ eAdd1, Add 5 <$ eAdd5, Total <$ eTotal, Clear <$ eClear]

  let
    eSnapshot = leftmost [eSnapshot', void . ffilter (10 <=) . updated . _lSize $ log]
    eVacuum = leftmost [eVacuum', _lSnapshotComplete log]

  log <- mkFileLog "testme" $ LogConfig calc 0 eCmd eSnapshot eVacuum

  el "div" $
    display $ _lValue log

  pure ()
