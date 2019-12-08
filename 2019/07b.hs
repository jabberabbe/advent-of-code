#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package csv-conduit --package bytestring --package vector --package mtl --package conduit --package transformers
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import qualified Data.ByteString           as B
import           Data.Conduit
import           Data.Conduit.Lift
import qualified Data.CSV.Conduit          as CSV
import           Data.IORef
import           Data.List
import qualified Data.Vector               as V
import           Debug.Trace

main = do
    v' <- either (error "csv error") id . CSV.decodeCSV CSV.defCSVSettings <$> B.getLine
    let v = V.map read . V.fromList . V.head $ v'
    let phases = permutations [5..9]
    signals <- forM phases $ \phase -> do
        outputE <- newIORef (0 :: Int)
        runMaybeT $ runConduit $  yieldInfiniteFromIORef outputE
                   .| foldl1 (.|) (map (runAmplifierAsConduit v) phase)
                   .| writeInfiniteToIORef outputE
        readIORef outputE
    print . maximum $ (signals :: [Int])

yieldInfiniteFromIORef ioref = liftIO (readIORef ioref) >>= yield >> yieldInfiniteFromIORef ioref
writeInfiniteToIORef   ioref = await >>= (\case { Nothing -> return (); Just x -> (liftIO $ writeIORef ioref x) >> writeInfiniteToIORef ioref})

runAmplifierAsConduit :: Monad m => V.Vector Int -> Int -> (ConduitT Int Int (MaybeT m) ()) -- phase -> state action
runAmplifierAsConduit program phase = leftover phase >> (flip evalStateT (program, 0) $ run (maybeC await) yield (lift mzero))

run :: Monad m => (m Int) -> (Int -> m ()) -> (m ()) -> StateT (V.Vector Int, Int) m ()
run read write end = do
    ix <- gets snd
    vec <- gets fst
    let opcode = vec V.! ix
    case opcode of
        99 -> lift end
        z -> do
            let str = (reverse . show $ z) ++ (repeat '0')
                opc = take 2 str
            case opc of
                "30" -> do
                    s <- lift read
                    let ixRes = vec V.! (ix+1)
                    modify $ \(v,ix) -> (v V.// [(ixRes, s)], ix+2)
                "40" -> do
                    lift . write =<< getParamN 0 str
                    modify $ \(v,ix) -> (v, ix+2)
                "50" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    -- let x1 = vec V.! (ix+2)
                    if (x0 /= 0) then
                        modify $ \(v,ix) -> (v, x1)
                    else
                        modify $ \(v,ix) -> (v, ix+3)
                "60" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    -- let x1 = vec V.! (ix+2)
                    if (x0 == 0) then
                        modify $ \(v,ix) -> (v, x1)
                    else
                        modify $ \(v,ix) -> (v, ix+3)
                "70" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    let ixRes = vec V.! (ix+3)
                    -- ixRes <- getParamN 2 str
                    modify $ \(v,ix) -> (v V.// [(ixRes, if x0 < x1 then 1 else 0)], ix+4)
                "80" -> do
                    x0 <- getParamN 0 str
                    x1 <- getParamN 1 str
                    let ixRes = vec V.! (ix+3)
                    -- ixRes <- getParamN 2 str
                    modify $ \(v,ix) -> (v V.// [(ixRes, if x0 == x1 then 1 else 0)], ix+4)
                op -> if op == "10" || op == "20"
                    then do
                        x0 <- getParamN 0 str
                        x1 <- getParamN 1 str
                        let ixRes = vec V.! (ix+3)
                        modify $ \(v,ix) -> (v V.// [(ixRes, if op == "10" then x0+x1 else x0*x1)], ix+4)
                    else
                        lift end
            run read write end

getParamN :: Monad m => Int -> [Char] -> StateT (V.Vector Int, Int) m Int
getParamN n str = do
    ix <- gets snd
    vec <- gets fst
    if (str !! (n+2)) == '1'
    then
        return $ vec V.! (ix+n+1)
    else do
        let ixRes = vec V.! (ix+n+1)
        return $ vec V.! ixRes
