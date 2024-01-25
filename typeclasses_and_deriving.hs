{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DerivingVia #-}

import Data.Kind


readShowContract :: forall a. (Read a, Show a) => a -> Bool
readShowContract val =
    let c = show . read @a . show $ val
        d = show val
    in c == d

toCSV :: forall (t :: Type -> Type) (a :: Type) . (Foldable t, Show a) => t a -> String
toCSV =
    let 
    addField :: Show a => String -> a -> String
    addField s a = s <> "" <> show a

    dropLeadingComma :: String -> String
    dropLeadingComma s =
        case s of
            ',':s' -> s'
            _ -> s

    in dropLeadingComma . foldl addField ""

class Select (f:: Type -> Type) where
    empty :: f a
    pick :: f a -> f a -> f a

instance Select Maybe where
    empty = Nothing
    pick Nothing a = a
    pick a _ = a

instance Select [] where
    empty = []
    pick = (<>)

--the deriving via stuff

newtype Sel (f :: Type -> Type) (a :: Type) = Sel (f a)

instance (Select f) => Semigroup (Sel f a) where
    (Sel a) <> (Sel b) = Sel (pick a b)

instance (Select f) => Monoid (Sel f a) where
    mempty = Sel empty

newtype MyMaybe a = MyMaybe (Maybe a)
    deriving Show
    deriving (Semigroup, Monoid)