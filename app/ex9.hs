module Ex9 where
    import Data.List 

    myeft :: (Enum a, Ord a) => a -> a -> [a]
    myeft a b 
        | a < b = a : myeft (succ a) b
        | a == b = [b]
        | a > b = []

    myWords :: Char -> String -> [String]
    myWords _ "" = []
    myWords sep s = takeWhile (/=sep) s : myWords sep (drop 1 (dropWhile (/=sep) s))

    mySqr = [x^2 | x <- [1..5]]
    myCube = [x^3 | x <- [1..5]]

    myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    myZipWith f [] _ = []
    myZipWith f _ [] = []
    myZipWith f xs ys =  f (head xs) (head ys) : myZipWith f (drop 1 xs) (drop 1 ys)
    
    myZip :: [a] -> [b] -> [(a,b)]
    myZip xs ys = myZipWith (,) xs ys

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f xs = (length $ filter f xs) > 0

    myOr :: [Bool] -> Bool
    myOr = myAny id

    myElm :: Eq a => a -> [a] -> Bool
    myElm x = myAny (==x) 

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f [] = []
    squishMap f (x:xs) = f x ++ squishMap f xs


    squish :: [[a]] -> [a]
    squish = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f [] = undefined
    myMaximumBy f (x:y:xs) = myMaximumBy f (nex:xs)
        where
            nex = if f x y == GT then x else y
    myMaximumBy f (x:nil) = x

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f [] = undefined
    myMinimumBy f (x:y:xs) = myMinimumBy f (nex:xs)
        where
            nex = if f x y == LT then x else y
    myMinimumBy f (x:nil) = x

    myMaximum :: Ord a => [a] -> a
    myMaximum = myMaximumBy compare

    myMinimum :: Ord a => [a] -> a
    myMinimum = myMinimumBy compare
