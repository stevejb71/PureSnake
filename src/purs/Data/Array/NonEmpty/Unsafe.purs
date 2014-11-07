module Data.Array.NonEmpty.Unsafe where

import Data.Array.NonEmpty

fromArray :: forall a. [a] -> NonEmpty a
fromArray (a:as) = a :| as