import Prelude hiding (null)

newtype Sum = Sum { getSum :: Int } deriving Show

instance Semigroup Sum where
    (Sum a) <> (Sum b) = Sum (a + b)

instance Monoid Sum where
    mempty = Sum 0

--creating record structure type class
data Natural a = Natural {
     equal :: a -> a -> Bool
    , add :: a -> a -> a
    , multiply :: a -> a -> a
    , additiveIdentity :: a
    , multiplicativeIdentity :: a
    , displayAsString :: a -> String
}

intNatural :: Natural Int
intNatural = Natural { 
    equal = (==)
    , add = (+)
    , multiply = (*)
    , additiveIdentity = 0
    , multiplicativeIdentity = 1
    , displayAsString = show
    }

--writing type classes representing emptiness


