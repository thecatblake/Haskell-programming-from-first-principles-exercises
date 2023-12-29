module Ex10 where
    import Data.Time 

    data DatabaseItem = DbString String 
                      | DbNumber Integer
                      | DbDate UTCTime
                      deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase = 
        [
            DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
            DbNumber 9001,
            DbString "hello world",
            DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
        ]

    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = foldr (\x y -> case x of
                                    DbDate t -> t : y
                                    _ -> y) []

    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber = foldr (\x y -> case x of 
                                        DbNumber i -> i : y
                                        _ -> y) []

    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = maximum . filterDbDate

    sumDb :: [DatabaseItem] -> Integer
    sumDb = foldr (\x y -> case x of 
                                        DbNumber i -> i + y
                                        _ -> y) 0

    average :: Fractional a => [a] -> a
    average xs = sum xs / fromIntegral (length xs)
    avgDb :: [DatabaseItem] -> Double
    avgDb = average . (map fromIntegral) . filterDbNumber

    myOr :: [Bool] -> Bool
    myOr = foldr (||) False

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f = foldr (\x y -> y || f x) False

    myElm :: Eq a => a -> [a] -> Bool
    myElm x = myAny (==x)

    myReverse :: [a] -> [a]
    myReverse = foldl (flip (:)) []

    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr ((:) . f) []

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = foldr (\x y -> if f x then x : y else y) []

    squish :: [[a]] -> [a]
    squish = foldr (++) []

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f = foldr ((++) . f) []

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (head xs) (tail xs)

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
    myMinimumBy f xs = foldr (\x y -> if f x y == LT then x else y) (head xs) (tail xs)
    