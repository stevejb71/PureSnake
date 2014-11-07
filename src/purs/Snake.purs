module Snake where

import Base
import Control.Monad.Eff
import Graphics.Canvas
import Data.Foldable
import Data.Array.NonEmpty

type CanvasEff a = forall e. Eff (canvas :: Canvas | e) a

type Position = {x :: Number, y :: Number}

showPosition :: Position -> String
showPosition {x = x, y = y} = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

type Board = {size :: Number, squares :: Number}
board = {size: 500, squares: 50}

data Direction = N | W | S | E 

instance showDirection :: Show Direction where
    show d = case d of 
        N -> "N"
        W -> "W"
        S -> "S"
        E -> "E"

data Snake = Snake Direction (NonEmpty Position) 

instance showSnake :: Show Snake where
    show (Snake d ps) = "Snake " ++ (show d) ++ " " ++ show (map showPosition ps)

snakeTail (Snake _ ps) = last ps

starterSnake = Snake S ({x:5,y:5} :| [{x:6,y:5},{x:6,y:6}])

changeDirection :: Direction -> Snake -> Snake
changeDirection d' (Snake _ ps) = Snake d' ps

moveSnake :: Snake -> Snake
moveSnake (Snake d l@(NonEmpty p ps)) = 
    let p' = case d of 
                N -> {x:p.x, y:p.y-1}
                S -> {x:p.x, y:p.y+1}
                W -> {x:p.x-1, y:p.y}
                E -> {x:p.x+1, y:p.y}
    in Snake d (p' :| (pop l)) 

toView :: Board -> Position -> Rectangle
toView b p = let sqSize = b.size / b.squares in {h:sqSize, w:sqSize, y:p.y*sqSize, x:p.x*sqSize}

drawSnake :: forall e. Context2D -> (Position -> Rectangle) -> Snake -> Eff (canvas :: Canvas | e) Unit
drawSnake ctx tr (Snake _ ps) = for_ (map tr ps) $ fillRect ctx

mkLoop :: Eff (canvas :: Canvas) (Snake -> Eff (canvas :: Canvas) Snake)
mkLoop = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    return $ loop ctx

loop :: Context2D -> Snake -> Eff (canvas :: Canvas) Snake
loop ctx s = do
    _ <- clearRect ctx $ (toView board) (snakeTail s)
    let s' = moveSnake s
    _ <- drawSnake ctx (toView board) s'
    return s'
