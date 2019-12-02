#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package csv-conduit --package bytestring --package vector --package mtl
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.State
import qualified Data.ByteString     as B
import qualified Data.CSV.Conduit    as CSV
import qualified Data.Vector         as V

main = do
    v' <- either (error "csv error") id . CSV.decodeCSV CSV.defCSVSettings <$> B.getContents
    let v = V.map read . V.fromList . V.head $ v'
    print . V.head . fst $ execState run (v V.// [(1,12),(2,2)], 0)

run :: State (V.Vector Int, Int) ()
run = do
    ix <- gets snd
    vec <- gets (fst :: (V.Vector Int, Int) -> V.Vector Int)
    let opcode = vec V.! ix
    case opcode of
        99 -> return ()
        z -> do
            let ix1 = vec V.! (ix+1)
                ix2 = vec V.! (ix+2)
                ixRes = vec V.! (ix+3)
                x1 = vec V.! ix1
                x2 = vec V.! ix2
            modify $ \(v,ix) -> (v V.// [(ixRes, if z == 1 then x1+x2 else x1*x2)], ix+4)
            run

