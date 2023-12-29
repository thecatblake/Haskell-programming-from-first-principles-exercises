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