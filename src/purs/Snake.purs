module Snake where

import Canvas
import Base

type Position = {x :: Number, y :: Number}

type Board = {x :: Number, y :: Number}

data Direction = N | W | S | E

data Snake = Snake Direction [Position]

changeDirection :: Direction -> Snake -> Snake
changeDirection d' (Snake _ ps) = Snake d' ps

moveSnake :: Board -> Snake -> Snake
moveSnake b (Snake d ps) = undefined

startSnake = do
    ctx <- canvasContext "canvas"
    drawRect ctx