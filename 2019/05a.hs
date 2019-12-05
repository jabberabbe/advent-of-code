#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package csv-conduit --package bytestring --package vector --package mtl
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.State
import qualified Data.ByteString     as B
import qualified Data.CSV.Conduit    as CSV
import qualified Data.Vector         as V
import           Debug.Trace

main = do
    v' <- either (error "csv error") id . CSV.decodeCSV CSV.defCSVSettings <$> B.getLine
    let v = V.map read . V.fromList . V.head $ v'
    runStateT run (v, 0)

run :: StateT (V.Vector Int, Int) IO ()
run = do
    ix <- gets snd
    vec <- gets fst
    let opcode = vec V.! ix
    case opcode of
        99 -> return ()
        z -> do
            let str = (reverse . show $ z) ++ (repeat '0')
                opc = take 2 str
            case opc of
                "40" -> do
                    lift . print =<< getParamN 0 str
                    modify $ \(v,ix) -> (v, ix+2)
                "30" -> do
                    s <- read <$> lift getLine
                    let ix = vec V.! (ix+1)
                    modify $ \(v,ix) -> (v V.// [(ix, s)], ix+2)
                op -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    let ixRes = vec V.! (ix+3)
                    modify $ \(v,ix) -> (v V.// [(ixRes, if op == "10" then x0+x1 else x0*x1)], ix+4)
            run

getParamN :: Int -> [Char] -> StateT (V.Vector Int, Int) IO Int
getParamN n str = do
    ix <- gets snd
    vec <- gets fst
    if (str !! (n+2)) == '1'
    then
        return $ vec V.! (ix+n+1)
    else do
        let ixRes = vec V.! (ix+n+1)
        return $ vec V.! ixRes
