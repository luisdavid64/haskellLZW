module Safe where 

import Data.Maybe

maybeHead :: [Char] -> Maybe [Char]
maybeHead [] = Nothing
maybeHead (x:xs) = Just [x]

safeHead :: [Char] -> [Char]
safeHead list = fromMaybe "" $ maybeHead list
