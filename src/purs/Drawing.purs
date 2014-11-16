module Drawing where

import Base
import Control.Monad.Eff (Eff())
import Graphics.Canvas (Canvas(), fillRect, setFillStyle, Context2D(), Rectangle())
import Data.Foldable (for_)
import Data.Array.NonEmpty (map, NonEmpty(..), toArray)
import Control.Monad.Eff.Random (Random())
import Random (randomN)

red = "rgb(200,10,10)"
black = "rgb(0,0,0)"

type ApplePosition = Position

drawSnake :: forall e. Context2D -> (Position -> Rectangle) -> Snake -> Eff (canvas :: Canvas | e) Unit
drawSnake ctx tr (Snake _ ps) = for_ (toArray $ map tr ps) $ fillRectWithStyle ctx black

drawApple :: Context2D -> ApplePosition -> (ApplePosition -> Rectangle) -> Eff (canvas :: Canvas, random :: Random) Unit
drawApple ctx pos tr = fillRectWithStyle ctx red (tr pos)

fillRectWithStyle :: forall e. Context2D -> String -> Rectangle -> Eff (canvas :: Canvas | e) Unit
fillRectWithStyle ctx style rect = do
    _ <- setFillStyle style ctx
    _ <- fillRect ctx rect
    return unit  
