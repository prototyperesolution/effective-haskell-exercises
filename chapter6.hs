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

class Eq a => Nullable a where
    isNull :: a -> Bool
    isNull a = a == null
    null :: a

instance Nullable a => Nullable (Maybe a) where
    isNull Nothing = True
    isNull _ = False
    null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
    isNull (a, b) = isNull a && isNull b
    null = (null, null)

instance Eq a => Nullable [a] where
    isNull [] = True
    isNull _ = False
    null = []

