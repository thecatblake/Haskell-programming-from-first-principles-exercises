module Ex15 where
    import Data.Monoid 
    import Test.QuickCheck

    data Optional a = 
        Nada |
        Only a 
        deriving (Eq, Show)

    
    instance Semigroup a => Semigroup (Optional a) where 
        (<>) (Only x) (Only y) = Only (x <> y)
        (<>) Nada (Only x) = Only x
        (<>) (Only x) Nada = Only x
        (<>) _ _ = Nada

    instance Monoid a => Monoid (Optional a) where
        mempty = Nada

    monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
    monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

    monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidLeftIdentity a = (mempty <> a) == a

    monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidRightIdentity a = (a <> mempty) == a

    data Trivial = Trivial deriving (Eq, Show)

    instance Semigroup Trivial where
        Trivial <> Trivial = Trivial
    
    instance Arbitrary Trivial where
        arbitrary = return Trivial
    
    semigroupAssoc :: (Eq m, Semigroup m) => m  -> m -> m -> Bool
    semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

    type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

    newtype Identity a = Identity a deriving (Eq, Show)

    instance (Semigroup a) => Semigroup (Identity a) where
        Identity x <> Identity y = Identity x

    instance (Arbitrary a) => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary
    
    type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

    data Two a b = Two a b deriving (Eq, Show)

    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')


    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Two a b
    
    type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

    newtype BoolConj = BoolConj Bool deriving (Eq, Show)

    instance Semigroup BoolConj where
        BoolConj True <> BoolConj True = BoolConj True
        _ <> _ = BoolConj False
    
    instance Arbitrary BoolConj where
        arbitrary = BoolConj <$> arbitrary

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

    instance Semigroup BoolDisj where
        BoolDisj False <> BoolDisj False = BoolDisj False
        _ <> _ = BoolDisj False

    instance Arbitrary BoolDisj where
        arbitrary = BoolDisj <$> arbitrary

    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
    
    data Or a b = 
        Fst a |
        Snd b
        deriving (Eq, Show)

    instance Semigroup (Or a b) where
        Fst x <> Fst y = Fst y
        Fst x <> Snd y = Snd y
        Snd x <> Fst y = Snd x
        Snd x <> Snd y = Snd x

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

    type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

    newtype Combine a b = 
        Combine { unCombine :: (a -> b) }

    instance Semigroup b => Semigroup (Combine a b) where
        Combine f <> Combine g = Combine (f <> g)

    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = Combine <$> arbitrary

    type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool
    
    newtype Comp a = Comp {unComp :: (a -> a)}

    instance Semigroup a => Semigroup (Comp a) where
        Comp f <> Comp g = Comp (f <> g)


    instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
        arbitrary = Comp <$> arbitrary

    type CompAssoc a = Comp a -> Comp a -> Comp a -> Bool
    
