#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package conduit --package text
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Conduit
import           Data.Text

main = (runConduit $
    stdinC
    .| decodeUtf8C
    .| takeCE 15000
    .| myFold 10
    .| minimumC) >>= (\(Just [a,b,c]) -> putStrLn . show $ b*c)

myFold :: Int -> ConduitT Text [Int] IO ()
myFold 0 = return ()
myFold i = do
    folded <- takeExactlyCE (25*6) $ foldlCE (\[num0,num1,num2] char ->
        case char of
            '0' -> [num0+1,num1,num2]
            '1' -> [num0,num1+1,num2]
            '2' -> [num0,num1,num2+1]
            _   -> [num0,num1,num2]) [0,0,0]
    yield folded
    myFold $ i-1
