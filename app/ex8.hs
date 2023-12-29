module Ex8 where
    import Data.List (intersperse)

    data DividedResut =
        Result Integer
        | DividedByZero
        deriving (Show)

    dividedBy :: Integer -> Integer -> DividedResut
    dividedBy _ 0 = DividedByZero
    dividedBy x y = go x y 0
        where go a   b cnt
                | a < b = Result cnt
                | otherwise = go (a - b) b (cnt+1)

    mc91 :: Integer -> Integer
    mc91 x
        | x > 100 = x - 10
        | otherwise = mc91 . mc91 $ (x + 11)

    digitToWord :: Integer -> String
    digitToWord 0 = "zero"
    digitToWord 1 = "one"
    digitToWord 2 = "two"
    digitToWord 3 = "three"
    digitToWord 4 = "four"
    digitToWord 5 = "five"
    digitToWord 6 = "six"
    digitToWord 7 = "seven"
    digitToWord 8 = "eight"
    digitToWord 9 = "nine"
    digitToWord _ = ""

    digits :: Integer -> [Integer]
    digits 0 = []
    digits x = digits d ++ [m]
        where
            (d, m) = divMod x 10

    wordNumber :: Integer -> String
    wordNumber x = concat $ intersperse "-" (map digitToWord (digits x))