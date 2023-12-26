module Ex11 where
    import Data.Int
    import Data.Char
    import Ex9

    data Doggies a =
        Husky a 
        | Mastiff a 
        deriving (Eq, Show)

    data Price = Price Integer deriving (Eq, Show)

    data Manufacturer = 
        Mini 
        | Mazda 
        | Tata 
        deriving (Eq, Show)

    data Airline = 
        PapuAir 
        | CatapultsR'Us
        | TakeYourChancesUnited
        deriving (Eq, Show)

    data Vehicle = Car Manufacturer Price
                | Plane Airline Integer
                deriving (Eq, Show)

    isCar :: Vehicle -> Bool
    isCar (Car _ (Price _)) = True
    isCar _   = False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _ _) = True
    isPlane _ = False

    areCars :: [Vehicle] -> Bool
    areCars = all isCar

    getManu :: Vehicle -> Manufacturer
    getManu (Car manu _) = manu
    getManu _ = undefined

    newtype Goats = Goats Int deriving (Show, TooMany)

    class TooMany a where 
        tooMany :: a -> Bool

    instance TooMany Int where
        tooMany n = n > 43


    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir

    data QuantumBool = QuantumTrue
                    | QuantumFale
                    | QuantumBoth deriving (Eq, Show)

    data TwoWs = 
        MkTwoWs QuantumBool QuantumBool
        deriving (Eq, Show)

    data Person = 
        Person { name :: String , age :: Int }
        deriving (Eq, Show)

    data Sum a b = 
        First a |
        Second b
        deriving (Eq, Show)

    data OperatingSystem = 
        GnuPlusLinux
        | OpenBSDPlusNevermindJustBSDStill
        | Mac 
        | Windows 
        deriving (Eq, Show)

    data ProgrammingLanguage = 
        Haskell
        | Agda 
        | Idris 
        | PureScript
        deriving (Eq, Show)

    data Programmer = 
        Programmer {os :: OperatingSystem,
                    lang:: ProgrammingLanguage}
        deriving (Eq, Show)

    allOperatingSystems :: [OperatingSystem]
    allOperatingSystems = 
        [
        GnuPlusLinux,
        OpenBSDPlusNevermindJustBSDStill,
        Mac,
        Windows
        ]
    allLanguages :: [ProgrammingLanguage]
    allLanguages = 
        [
            Haskell,
            Agda,
            Idris,
            PureScript
        ]

    myIsSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    myIsSubsequenceOf [] _ = True
    myIsSubsequenceOf _ [] = False
    myIsSubsequenceOf t@(x:xs) (y:ys) 
        | x == y = myIsSubsequenceOf xs ys
        | otherwise = myIsSubsequenceOf t ys
    
    capitalizeWords :: String -> [(String, String)]
    capitalizeWords s = map (\t@(x:xs) -> (t, toUpper x : xs)) (myWords ' ' s)

    capitalizeWord :: String -> String
    capitalizeWord (x:xs) = toUpper x : xs

    capitalizeParagraph' :: String -> Bool -> String
    capitalizeParagraph' [] _ = []
    capitalizeParagraph' ('.':xs) _ = '.' : capitalizeParagraph' xs True
    capitalizeParagraph' (' ':xs) t = ' ': capitalizeParagraph' xs t
    capitalizeParagraph' (x:xs) True = toUpper x : capitalizeParagraph' xs False
    capitalizeParagraph' (x:xs) False = x : capitalizeParagraph' xs False

    capitalizeParagraph :: String -> String
    capitalizeParagraph (x:xs) = toUpper x : (capitalizeParagraph' xs False)

    data Expr 
        = Lit Integer 
        | Add Expr Expr

    eval :: Expr -> Integer
    eval (Lit n) = n 
    eval (Add e e') = eval e + eval e'

    printExpr :: Expr -> String
    printExpr (Lit n) = show n
    printExpr (Add e e') = printExpr e ++ " + " ++ printExpr e'