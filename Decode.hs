module Decode where 

import Data.Char
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe
import Safe

--Produces a dictionary of strings
strings :: Map.Map Int String
strings = Map.fromList(map getAscii [0..255])
   where getAscii n =  (n, [chr n])

--Main functionality for uncompression
decodeIO :: [Int] -> String -> Map.Map Int String -> Int -> IO()
decodeIO [] _ _ _ = return ()
decodeIO (x:xs) prev dict nextCode = do
   if (x `Map.notMember` dict) 
       then do
           let newDict = Map.insert x (prev ++ safeHead prev) dict
           putStr $ fromMaybe "" (Map.lookup x newDict)
           composeEntry (x:xs) prev newDict nextCode
       else do
           putStr $ fromMaybe "" (Map.lookup x dict)
           composeEntry (x:xs) prev dict nextCode

composeEntry :: [Int] -> String -> Map.Map Int String -> Int -> IO()
composeEntry (x:xs) prev dict nextCode = do
    if (not $ null prev)
        then do
          let newDict = Map.insert nextCode (prev ++ safeHead (fromMaybe "" (Map.lookup x dict))) dict
              nextCodeVal = nextCode+1          
              prevVal = fromMaybe "" (Map.lookup x dict)
          decodeIO xs prevVal newDict nextCodeVal
        else do
          let prevVal = fromMaybe "" (Map.lookup x dict)
          decodeIO xs prevVal dict nextCode
           
--Displays protocol for uncompressing a file
decodeReader :: IO()
decodeReader = do
   putStrLn "What file to uncompress?"
   file <- getLine
   contents <- readFile file
   let nums = (map read $ words contents) :: [Int]
   decodeIO nums [] strings 257
   putStrLn ""

