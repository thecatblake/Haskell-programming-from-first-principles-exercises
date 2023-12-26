module Ex6 where
  import Data.List

  data Person = Person Bool deriving Show

  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  data Mood = Blah |
              Woot deriving (Show, Eq)

  settleDown :: Mood -> Mood
  settleDown x = if x == Woot
                   then Blah
                   else x

  data Rocks =
    Rocks String deriving (Eq, Show)

  data Yeah =
    Yeah Bool deriving (Eq, Show)

  data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

  equalityForAll :: Papu -> Papu -> Bool
  equalityForAll p p' = p == p'


  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk f a b = f a == b

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith f x a = f a + fromInteger x