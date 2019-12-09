#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package csv-conduit --package bytestring --package vector --package mtl --package lens
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.ByteString      as B
import qualified Data.CSV.Conduit     as CSV
import           Data.List
import qualified Data.Vector          as V
import           Debug.Trace

main = do
    v' <- either (error "csv error") id . CSV.decodeCSV CSV.defCSVSettings <$> B.getLine
    let v = V.map read . V.fromList . V.head $ v'
    evalStateT (run (read <$> getLine) print) (v, 0, 0)

run :: Monad m => (m Int) -> (Int -> m ()) -> StateT (V.Vector Int, Int, Int) m ()
run read write = do
    ix <- use _2
    vec <- use _1
    opcode <- safeIndex ix
    case opcode of
        99 -> return ()
        z -> do
            let str = (reverse . show $ z) ++ (repeat '0')
                opc = take 2 str
            case opc of
                "30" -> do
                    s <- lift read
                    ixRes <- getWriteIndexForParamN 0 str
                    _1 %= (V.// [(ixRes,s)])
                    _2 += 2
                "40" -> do
                    lift . write =<< getParamN 0 str
                    _2 += 2
                "50" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    if (x0 /= 0) then
                        _2 .= x1
                    else
                        _2 += 3
                "60" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    if (x0 == 0) then
                        _2 .= x1
                    else
                        _2 += 3
                "70" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    ixRes <- getWriteIndexForParamN 2 str
                    _1 %= (V.// [(ixRes, if x0 < x1 then 1 else 0)])
                    _2 += 4
                "80" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    ixRes <- getWriteIndexForParamN 2 str
                    _1 %= (V.// [(ixRes, if x0 == x1 then 1 else 0)])
                    _2 += 4
                "90" -> do
                    adjust <- getParamN 0 str
                    _2 += 2
                    _3 += adjust
                op -> if op == "10" || op == "20"
                    then do
                        x0 <- getParamN 0 str
                        x1 <- getParamN 1 str
                        ixRes <- getWriteIndexForParamN 2 str
                        _1 %= (V.// [(ixRes, if op == "10" then x0+x1 else x0*x1)])
                        _2 += 4
                    else
                        return ()
            run read write

getParamN :: Monad m => Int -> [Char] -> StateT (V.Vector Int, Int, Int) m Int
getParamN n str = do
    ix <- use _2
    vec <- use _1
    if (str !! (n+2)) == '1'
    then
        safeIndex (ix+n+1)
    else if (str !! (n+2)) == '0' then do
        ixRes <- safeIndex (ix+n+1)
        safeIndex ixRes
    else do
        rbase <- use _3
        ixRes <- safeIndex (ix+n+1)
        safeIndex (rbase+ixRes)

getWriteIndexForParamN n str = do
    ix <- use _2
    vec <- use _1
    if (str !! (n+2)) == '0' then do
        ixRes <- safeIndex (ix+n+1)
        _ <- safeIndex ixRes -- ensure the array is long enough
        return ixRes
    else do
        rbase <- use _3
        ixRes <- safeIndex (ix+n+1)
        _ <- safeIndex $ rbase+ixRes
        return (rbase+ixRes)

safeIndex :: Monad m => Int -> StateT (V.Vector Int, Int, Int) m Int
safeIndex ix = do
    v <- use _1
    if (V.length v > ix)
    then
      return $ v V.! ix
    else do
      _1 %= (V.++ (V.replicate (ix-(V.length v)+1) 0))
      return 0
