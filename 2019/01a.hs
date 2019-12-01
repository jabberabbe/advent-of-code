#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package conduit --package csv-conduit
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Conduit
import           Data.CSV.Conduit


main = (runConduit $
    stdinC
    .| intoCSV defCSVSettings
    .| mapC (head . map read)
    .| mapC (((-2)+).(`div` 3))
    .| sumC) >>= ((putStrLn . show) :: Int -> IO ())
