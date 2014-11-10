module Base where

import Data.Array.NonEmpty (NonEmpty())

foreign import undefined :: forall a. a

type Position = {x :: Number, y :: Number}

eqPosition :: Position -> Position -> Boolean
eqPosition p1 p2 = p1.x == p2.x && p1.y == p2.y

notEqPosition p1 p2 = not (p1 `eqPosition` p2)

data Direction = N | W | S | E 

data Snake = Snake Direction (NonEmpty Position) 