module Encode where 

import Data.Char
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe
--Maps are left-biased

--Produces a starting dictionary of codes
codes :: Map.Map String Int
codes = Map.fromList(map getAscii [0..255])
   where getAscii n =  ([chr n], n)

--Core functionality for compression
encode :: String -> String -> Map.Map String Int -> Int -> IO ()
encode [] _ _ _ = return ()
encode (x:xs) accum dict nextCode 
    | (accum ++ [x]) `Map.notMember` dict = do  putStr . show $ (fromMaybe 0  (Map.lookup accum dict))
                                                putStr " "
                                                encode xs ([x]) (Map.insert (accum ++ [x]) nextCode dict) (nextCode + 1)
    | otherwise = encode xs (accum ++ [x]) dict nextCode

--Displays protocol for compressing a file
encodeReader :: IO()
encodeReader = do
    putStrLn "What file to compress?"
    file <- getLine
    contents <- readFile file
    encode contents [] codes 257
    putStrLn ""

