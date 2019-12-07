#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package csv-conduit --package bytestring --package vector --package mtl
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    let phases = permutations [0,1,2,3,4]
        signals = map (runAmplifiers v 0) phases
    print . maximum $ (signals :: [Int])

runAmplifiers :: V.Vector Int -> Int -> [Int] -> Int -- program -> input -> phase settings -> output
runAmplifiers _       input []       = input
runAmplifiers program input (ph:phs) = flip (runAmplifiers program) phs $ getSum $ flip evalState [ph, input] $ execWriterT $ evalStateT (run ((lift $ gets head) <* (lift $ modify tail)) (\x -> tell . Sum $ x)) (program, 0)

run :: Monad m => (m Int) -> (Int -> m ()) -> StateT (V.Vector Int, Int) m ()
run read write = do
    ix <- gets snd
    vec <- gets fst
    let opcode = vec V.! ix
    case opcode of
        99 -> return ()
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
                        return ()
            run read write

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
