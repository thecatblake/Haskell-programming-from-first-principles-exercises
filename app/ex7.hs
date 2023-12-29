module Ex7 where
    functionC :: Ord a => a -> a -> a
    functionC x y = 
        case x > y of 
            True -> x
            False -> y

    ifEvenAdd2 :: Integer -> Integer
    ifEvenAdd2 x = 
        case even x of
            True -> x + 2
            False -> x


    data Employee = Coder | 
                    Manager |
                    Veep |
                    CEO 
                    deriving (Eq, Ord, Show)

    reportBoss :: Employee ->Employee -> IO ()
    reportBoss e e' = 
        putStrLn $ show e ++ " is the boss of " ++ show e' 

    employeeRank :: (Employee -> Employee -> Ordering)
                    -> Employee
                    -> Employee
                    -> IO  ()
    employeeRank f e e' = 
        case f e e' of 
            GT -> reportBoss e e'
            EQ -> putStrLn "Neighr employee is the boss"
            LT -> (flip reportBoss) e e'

    myAbs :: Integer -> Integer
    myAbs x 
        | x < 0 = 0 - x
        | otherwise = x

    tenseDigit :: Integral a => a -> a
    tenseDigit x = d
        where (d, _) = divMod x 10

    g :: (a -> b) -> (a, c) -> (b, c)
    g f (x, y) = (f x , y)

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show