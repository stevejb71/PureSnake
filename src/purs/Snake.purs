module Snake where

import Canvas
import Base
import Control.Monad.Eff
import Graphics.Canvas

type Position = {x :: Number, y :: Number}

type Board = {x :: Number, y :: Number}

data Direction = N | W | S | E

data Snake = Snake Direction [Position]

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

rectAt :: Position -> Context2D -> Eff (c :: Canvas) Unit
rectAt = undefined

startSnake = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    _ <- fillRect ctx {h:10, w:10, y:10, x:20}
    return Unit