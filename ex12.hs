module Ex12 where
    import Data.Char
    import Data.List

    type Name = String
    type Age = Integer
    type ValidatePerson a = Either [PersonInvalid] a
    data Person = Person Name Age deriving Show

    data PersonInvalid = NameEmpty 
                        | AgeTooLow
                        deriving (Eq, Show)

    mkPerson :: Name
            -> Age
            -> ValidatePerson Person

    mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

    mkPerson' :: ValidatePerson Name
                -> ValidatePerson Age
                -> ValidatePerson Person

    mkPerson' (Right nameOk) (Right ageOk) = Right $ Person nameOk ageOk
    mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
    mkPerson' (Left badName) _ = Left badName
    mkPerson' _ (Left badAge) = Left badAge
    
    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = case age >= 0 of 
        True -> Right age
        False -> Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = case name /= "" of
        True -> Right name
        False -> Left [NameEmpty]
    
    replaceThe :: String -> String
    replaceThe s = (intercalate " " . (map (\x -> if x == "the" then "a" else x)) . words) s

    vowels :: [Char]
    vowels = "aeiuo"

    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel = foldr (\x y -> if elem (head x) vowels then y+1 else y) 0 . words 

    countVowels :: String -> Integer
    countVowels = foldr (\x y -> if elem x vowels then y+1 else y) 0

    data Nat =
        Zero
        | Succ Nat 
        deriving (Eq, Show)

    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ x) = natToInteger x + 1

    integerToNat :: Integer -> Maybe Nat
    integerToNat x
        | x == 0 = Just Zero
        | x < 0 = Nothing
        | x > 0 = case integerToNat (x-1) of
                    Just n -> Just $ Succ n
                    _ -> Nothing

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing = False

    isNoting :: Maybe a -> Bool
    isNoting = not . isJust

    mayybee :: b -> (a -> b) -> Maybe a -> b 
    mayybee _ f (Just x) = f x
    mayybee x _ Nothing = x

    fromMaybe :: a -> Maybe a -> a
    fromMaybe _ (Just x) = x 
    fromMaybe x Nothing = x 

    listToMaybe :: [a] -> Maybe a
    listToMaybe (x:xs) = Just x 
    listToMaybe [] = Nothing

    maybeToList :: Maybe a -> [a]
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\x y -> case x of 
                                Just v -> v : y
                                Nothing -> y) []

    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe xs = case (concat . (map maybeToList)) xs of 
                    [] -> Nothing
                    ys -> Just ys

    lefts' :: [Either a b] -> [a]
    lefts' = foldr (\x y -> case x of 
                                Left t -> t : y
                                Right _ -> y) []

    rights' :: [Either a b] -> [b]
    rights' = foldr (\x y -> case x of 
                                Left _ ->  y
                                Right t -> t: y) []

    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers = foldr (\x (a, b) -> case x of 
                                            Left t -> (t:a, b)
                                            Right t -> (a, t:b)) ([], [])
    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c 
    eitherMaybe' _ (Left _) = Nothing
    eitherMaybe' f (Right x) = Just $ f x

    either' :: (a -> c) -> (b -> c) -> Either a b -> c 
    either' f _ (Left x) = f x 
    either' _ f (Right x) = f x

    myIterate :: (a -> a) -> a -> [a]
    myIterate f x = y : (myIterate f y)
                where 
                    y = f x
    
    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr f x = case f x of 
                        Nothing -> []
                        Just (a, b) -> a : (myUnfoldr f b)

    betterIterate :: (a -> a) -> a -> [a]
    betterIterate f = myUnfoldr (\b -> Just (b, f b))

    data BinaryTree a =
        Leaf 
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

    unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b 
    unfold f x = case f x of 
                    Nothing -> Leaf
                    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

    treeBuild :: Integer -> BinaryTree Integer
    treeBuild n = unfold (\a -> if a == n then Nothing else Just (a+1, a, a+1)) 0