module Snake where

import Base
import Control.Monad.Eff
import Graphics.Canvas
import Data.Foldable
import Data.Array.NonEmpty
import Data.Maybe

type CanvasEff a = forall e. Eff (canvas :: Canvas | e) a

type Position = {x :: Number, y :: Number}

type KeyCode = Number

showPosition :: Position -> String
showPosition {x = x, y = y} = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

type Board = {w :: Number, h :: Number, squares :: Number}

data Direction = N | W | S | E 

instance showDirection :: Show Direction where
    show d = case d of 
        N -> "N"
        W -> "W"
        S -> "S"
        E -> "E"

data Snake = Snake Direction (NonEmpty Position) 

instance showSnake :: Show Snake where
    show (Snake d body) = "Snake " ++ (show d) ++ " " ++ show (map showPosition body)

snakeTail :: Snake -> Position
snakeTail (Snake _ body) = last body

isSnakeOutsideBoard :: Board -> Snake -> Boolean
isSnakeOutsideBoard b (Snake _ (NonEmpty {x=x,y=y} _)) = x < 0 || y < 0 || x >= b.squares || y >= b.squares

starterSnake :: Snake
starterSnake = Snake S ({x:5,y:5} :| [{x:6,y:5}])

changeDirection :: Direction -> Snake -> Snake
changeDirection d' (Snake _ ps) = Snake d' ps

moveSnake :: Direction -> Snake -> Snake
moveSnake d (Snake _ l@(NonEmpty p ps)) = 
    let p' = case d of 
                N -> {x:p.x, y:p.y-1}
                S -> {x:p.x, y:p.y+1}
                W -> {x:p.x-1, y:p.y}
                E -> {x:p.x+1, y:p.y}
    in Snake d (p' :| (pop l)) 

toView :: Board -> Position -> Rectangle
toView b p = { h: sqHeight, w: sqWidth, x: p.x * sqWidth, y: p.y * sqHeight }
    where sqWidth = b.w / b.squares 
          sqHeight = b.h / b.squares 

drawSnake :: forall e. Context2D -> (Position -> Rectangle) -> Snake -> Eff (canvas :: Canvas | e) Unit
drawSnake ctx tr (Snake _ ps) = for_ (map tr ps) $ fillRect ctx

keyToDirection :: KeyCode -> Maybe Direction
keyToDirection k = 
    case k of
        37 -> Just W
        39 -> Just E
        40 -> Just S
        38 -> Just N
        _ -> Nothing

type Result = {snake :: Snake, crashed :: Boolean}

mkLoop :: Eff (canvas :: Canvas) (KeyCode -> Snake -> Eff (canvas :: Canvas) Result)
mkLoop = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    let board = {w: 800, h: 800, squares: 40}
    return $ loop board ctx

loop :: Board -> Context2D -> KeyCode -> Snake -> Eff (canvas :: Canvas) Result
loop board ctx keyCode s@(Snake d body) = 
    let d' = fromMaybe d (keyToDirection keyCode)
        s' = moveSnake d' s in 
        if isSnakeOutsideBoard board s' 
        then return $ {snake: s', crashed: true}
        else do
            _ <- clearRect ctx $ (toView board) (snakeTail s)
            _ <- drawSnake ctx (toView board) s'
            return $ {snake: s', crashed: false}
