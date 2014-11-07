module Base where

import Data.Array.NonEmpty (NonEmpty())

foreign import undefined :: forall a. a

type Position = {x :: Number, y :: Number}

data Direction = N | W | S | E 

data Snake = Snake Direction (NonEmpty Position) 