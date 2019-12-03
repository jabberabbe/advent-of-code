#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package csv-conduit --package bytestring --package mtl --package lens --package vector --package containers
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString     as B
import qualified Data.CSV.Conduit    as CSV
import           Data.Foldable
import           Data.List
import qualified Data.Set            as S
import qualified Data.Vector         as V
import           Debug.Trace

for = flip map

data WirePiece = X Int | Y Int deriving (Show, Read)

main = do
    wires@[wire1', wire2'] <- V.toList . either (error "csv error") id . CSV.decodeCSV CSV.defCSVSettings <$> B.getContents
    let [wire1, wire2] = for wires $ map $ \case
         ('U':x) -> Y .          read $ x
         ('D':x) -> Y . negate . read $ x
         ('L':x) -> X . negate . read $ x
         ('R':x) -> X .          read $ x
         x -> error . show $ x
    let coords1 = traceShowId . snd . flip execState ((0,0),S.empty) . mapM myFold $ wire1
        coords2 = traceShowId . snd . flip execState ((0,0),S.empty) . mapM myFold $ wire2
    print . head . sort . map (uncurry (+) . mapTuple abs abs) . traceShowId . S.toList $ S.intersection coords1 coords2

myFold :: WirePiece -> State ((Int,Int), S.Set (Int, Int)) ()
myFold piece = do
    pos <- gets fst
    case piece of
        X m -> do
            let newPos = [mapTuple (+i) id pos | i <- [(min (signum m) m)..(max (signum m) m)]]
            mapM (\x -> modify $ \(a,b) -> (a, S.insert x b)) newPos
            _1 . _1 += m
        Y m -> do
            let newPos = [mapTuple id (+i) pos | i <- [(min (signum m) m)..(max (signum m) m)]]
            mapM (\x -> modify $ \(a,b) -> (a, S.insert x b)) newPos
            _1 . _2 += m


mapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f g (a1, a2) = (f a1, g a2)
