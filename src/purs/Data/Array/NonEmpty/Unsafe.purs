module Data.Array.NonEmpty.Unsafe where

import Data.Array.NonEmpty

fromList :: forall a. [a] -> NonEmpty a
fromList (a:as) = a :| as