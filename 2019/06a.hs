#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package unordered-containers --package mtl --package lens
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.List
import           Debug.Trace
import           System.IO

main = do
    (set, map) <- execStateT buildMap (S.empty, M.empty)
    let total = S.foldl' (myFold map) 0 set
    print total

myFold :: M.HashMap String String -> Int -> String -> Int
myFold map acc obj =
    let center = map M.! obj in
    if (center /= "COM")
    then
        myFold map (acc+1) center
    else
        acc+1

buildMap :: StateT (S.HashSet String, M.HashMap String String) IO ()
buildMap = do
    (center, _:object) <- span (/=')') <$> lift getLine
    _1 %= S.insert object
    _2 %= M.insert object center
    finished <- lift isEOF
    unless finished buildMap
