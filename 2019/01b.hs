#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package conduit --package csv-conduit
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Conduit
import           Data.CSV.Conduit
import           Data.List


main = (runConduit $
    stdinC
    .| intoCSV defCSVSettings
    .| mapC (head . map read)
    .| mapC calcFuel
    .| sumC) >>= ((putStrLn . show) :: Int -> IO ())
  where
    calcFuel = sum . tail . unfoldFuels
    unfoldFuels  = unfoldr $ \acc ->
        if (acc <= 0) then Nothing
        else
            Just (acc, (acc `div` 3)-2)
