module Data.Array.NonEmpty where

import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU

data NonEmpty a = NonEmpty a [a]

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
    show (NonEmpty a as) = (show a) ++ " :| " ++ (show as)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
    (==) (NonEmpty l ls) (NonEmpty r rs) = l == r && ls == rs
    (/=) l r = not (l == r)

instance functorNonEmpty :: Functor NonEmpty where
    (<$>) f as = map f as

infix 5 :|
(:|) :: forall a. a -> [a] -> NonEmpty a
(:|) a as = NonEmpty a as

toArray :: forall a. NonEmpty a -> [a]
toArray (NonEmpty a as) = a:as

head :: forall a. NonEmpty a -> a
head (NonEmpty a _) = a

tail :: forall a. NonEmpty a -> [a]
tail (NonEmpty _ as) = as

last :: forall a. NonEmpty a -> a
last (NonEmpty a []) = a
last (NonEmpty _ as) = AU.last as

(<|) :: forall a. a -> NonEmpty a -> NonEmpty a
(<|) a as = a :| toArray as

take :: forall a. Number -> NonEmpty a -> [a]
take 0 _ = []
take 1 (NonEmpty a _) = [a]
take n (NonEmpty a as) = a:(A.take (n - 1) as)

drop :: forall a. Number -> NonEmpty a -> [a]
drop 0 nel = toArray nel
drop 1 (NonEmpty _ as) = as
drop n (NonEmpty _ as) = A.drop (n-1) as

map :: forall a b. (a -> b) -> NonEmpty a -> NonEmpty b
map f (NonEmpty a as) = f a :| (A.map f as)

filter :: forall a. (a -> Boolean) -> NonEmpty a -> [a]
filter p as = A.filter p (toArray as)