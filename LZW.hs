module LZW where

import qualified Encode as E
import qualified Decode as D
import Control.Monad
import Data.Char

--Main function handling compression options
main = do
   putStrLn "Compress(C) or Uncompress(U)?"
   input <- getLine
   case input of
      [c] -> case toUpper c of
               'C'        -> E.encodeReader
               'U'        -> D.decodeReader
               otherwise -> putStrLn "Please choose one of the options above" >> main 
      otherwise -> putStrLn "Please choose one of the options above" >> main