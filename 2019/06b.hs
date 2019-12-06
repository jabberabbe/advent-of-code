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
    let pathYou = reverse $ takeWhile (/="COM") $ iterate (map M.!) "YOU"
        pathSan = reverse $ takeWhile (/="COM") $ iterate (map M.!) "SAN"
        (pathYou', pathSan') = removePrefix pathYou pathSan
    print $ length pathYou' + length pathSan' - 2 -- the two lists include YOU and SAN

removePrefix :: Eq a => [a] -> [a] -> ([a], [a])
removePrefix x@(x0:xt) y@(y0:yt) | x0 == y0 = removePrefix xt yt
                                 | otherwise = (x,y)

buildMap :: StateT (S.HashSet String, M.HashMap String String) IO ()
buildMap = do
    (center, _:object) <- span (/=')') <$> lift getLine
    _1 %= S.insert object
    _2 %= M.insert object center
    finished <- lift isEOF
    unless finished buildMap
