module Cipher where
    import Data.Char

    caesar :: Int -> String -> String
    caesar n s = map (chr . (\x -> 97 + (mod (n + max 0 (x - 97)) 26)) . ord) s 

    uncaesar :: Int -> String -> String 
    uncaesar n s = map (chr . (\x -> 97 + (mod (max 0 (x - 97) - n) 26)) . ord) s 

    shiftS :: Int -> Char -> Char
    shiftS i c = chr $ mod (ord c - 65 + i) 26 + 65

    vigenere' :: [Int] -> Int -> String -> Int -> String
    vigenere' _ _ [] _ = []
    vigenere' key keylen (' ':xs) cnt = ' ' : vigenere' key keylen xs cnt
    vigenere' key keylen (x:xs) cnt = shiftS (key !! mod cnt keylen) x : vigenere' key keylen xs (cnt+1)

    vigenere :: String -> String -> String
    vigenere key s = vigenere' keycodes keycodesLen s 0
        where
            keycodes = map ((+(-65)) . ord) key 
            keycodesLen = length keycodes
