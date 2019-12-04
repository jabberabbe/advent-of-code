#!/usr/bin/env stack
-- stack script --resolver lts-13.26
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Foldable
import           Data.List
import qualified Data.Set      as S
import           Debug.Trace

main = do
    bottom <- (read <$> getLine) :: IO Int
    top <- (read <$> getLine) :: IO Int
    print $ length . filter predicate . map show $ [bottom..top]

predicate :: [Char] -> Bool
predicate x = (x == sort x) && any (\(a:b:_) -> a==b) (slidingWindow2 x)

slidingWindow2 (l0:(l@(l1:_))) = [l0,l1]:(slidingWindow2 l)
slidingWindow2 _ = []
