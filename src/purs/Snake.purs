module Snake where

import Base
import Control.Monad.Eff
import Graphics.Canvas
import Data.Foldable
import Control.Monad.Eff.Ref
import Data.Array

type CanvasEff a = forall e. Eff (canvas :: Canvas | e) a

type Position = {x :: Number, y :: Number}

type Board = {size :: Number, squares :: Number}
board = {size: 500, squares: 50}

data Direction = N | W | S | E

data Snake = Snake Direction [Position]
starterSnake = Snake S [{x:5,y:5},{x:6,y:5},{x:6,y:6}]

changeDirection :: Direction -> Snake -> Snake
changeDirection d' (Snake _ ps) = Snake d' ps

moveSnake :: Board -> Snake -> Snake
moveSnake b (Snake d (p:ps)) = 
    let p' = case d of 
                N -> {x:p.x, y:p.y-1}
                S -> {x:p.x, y:p.y+1}
                W -> {x:p.x-1, y:p.y}
                E -> {x:p.x+1, y:p.y}
    in Snake d (p':(pop ps)) 

toView :: Board -> Position -> Rectangle
toView b p = let sqSize = b.size / b.squares in {h:sqSize, w:sqSize, y:p.y*sqSize, x:p.x*sqSize}

drawSnake :: forall e. Context2D -> (Position -> Rectangle) -> Snake -> Eff (canvas :: Canvas | e) Unit
drawSnake ctx tr (Snake _ ps) = for_ (map tr ps) $ fillRect ctx

mkLoop :: Eff (ref :: Ref, canvas :: Canvas) (Eff (ref :: Ref, canvas :: Canvas) Unit)
mkLoop = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    sr <- newRef starterSnake
    return $ loop ctx sr

loop :: Context2D -> RefVal Snake -> Eff (canvas :: Canvas, ref :: Ref) Unit
loop ctx sr = do
    s <- readRef sr
    _ <- drawSnake ctx (toView board) s
    _ <- writeRef sr $ moveSnake board s
    return unit
