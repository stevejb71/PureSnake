module Base where

import Data.Array.NonEmpty (NonEmpty())

foreign import undefined :: forall a. a

type Position = {x :: Number, y :: Number}

eqPosition :: Position -> Position -> Boolean
eqPosition p1 p2 = p1.x == p2.x && p1.y == p2.y

notEqPosition p1 p2 = not (p1 `eqPosition` p2)

data Direction = N | W | S | E 

instance eqDirection :: Eq Direction where
    (==) d1 d2 = eq d1 d2
    (/=) d1 d2 = not (d1 == d2)

foreign import eq
  """
  function eq(d1) {
    return function(d2) {
        return d1 === d2;
    }
  }
  """ :: forall a .a -> a -> Boolean

opposite :: Direction -> Direction
opposite d = case d of
    N -> S
    S -> N
    W -> E
    E -> W

data Snake = Snake Direction (NonEmpty Position) 