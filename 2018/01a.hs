#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package conduit --package csv-conduit
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
import           Conduit
import           Data.CSV.Conduit


main = runConduit $
    stdinC
    .| decodeUtf8C
    .| filterCE (/='+')
    .| encodeUtf8C
    .| intoCSV defCSVSettings
    .| mapC (head . map read)
    .| (sumC >>= lift . print)
