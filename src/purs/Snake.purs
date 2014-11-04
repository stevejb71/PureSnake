module Snake where

import Base
import Control.Monad.Eff
import Graphics.Canvas
import Data.Foldable
import Control.Monad.Eff.Ref

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

fillRectsAt :: forall e. Context2D -> Board -> [Position] -> Eff (canvas :: Canvas | e) Unit
fillRectsAt ctx b ps = for_ ps $ \p -> fillRect ctx (toView b p)

drawSnake :: forall e. Context2D -> Board -> Snake -> Eff (canvas :: Canvas | e) Unit
drawSnake ctx b (Snake _ ps) = fillRectsAt ctx b ps

startSnake = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    _ <- drawSnake ctx board starterSnake
    return Unit 

mkLoop = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    sr <- newRef starterSnake
    return $ loop ctx sr

loop :: Context2D -> RefVal Snake -> Eff (canvas :: Canvas, ref :: Ref) Unit
loop ctx sr = do
    s <- readRef sr
    _ <- drawSnake ctx board s
    _ <- writeRef sr $ moveSnake board s
    return unit
