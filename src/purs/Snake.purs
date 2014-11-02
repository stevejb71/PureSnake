module Snake where

import Base
import Control.Monad.Eff
import Graphics.Canvas
import Data.Traversable

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

fillRectAt :: Context2D -> Board -> Position -> Eff (canvas :: Canvas) Context2D
fillRectAt ctx b p = fillRect ctx (toView b p)

fillRectsAt :: Context2D -> Board -> [Position] -> Eff (canvas :: Canvas) [Context2D]
fillRectsAt ctx b ps = for ps (fillRectAt ctx b)

drawSnake :: Context2D -> Board -> Snake -> Eff (canvas :: Canvas) [Context2D]
drawSnake ctx b (Snake _ ps) = fillRectsAt ctx b ps

startSnake = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    _ <- drawSnake ctx board starterSnake
    return Unit