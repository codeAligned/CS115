import Data.Char
isInteger :: String -> Bool
isInteger st
      | length st == 1 = isNumber $ head st
      | otherwise = if isNumber (head st) == True then isInteger (tail st) else False 