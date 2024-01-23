{-# LANGUAGE KindSignatures #-}
module Selector where
import Data.Kind

class Select (f :: Type -> Type) where
    empty :: f a
    pick :: f a -> f a -> f a

instance Select Maybe where
    empty = Nothing
    pick Nothing a = a
    pick a _ = a

instance Select [] where
    empty = []
    pick a b = a <> b