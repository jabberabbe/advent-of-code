#!/usr/bin/env stack
-- stack script --resolver lts-13.26
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.List

main = do
    bottom <- (read <$> getLine) :: IO Int
    top <- (read <$> getLine) :: IO Int
    print $ length . filter predicate . map show $ [bottom..top]

predicate :: [Char] -> Bool
predicate x = (x == sort x) && any (\i -> (x !! i) == (x !! (i+1)) && (if i > 0 then (x !! (i-1)) /= (x !! i) else True) && (if i <= 3 then (x !! i) /= (x !! (i+2)) else True)) [0..4]
