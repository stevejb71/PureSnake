module Snake where

import Canvas

startSnake = do
    ctx <- canvasContext "canvas"
    drawRect ctx