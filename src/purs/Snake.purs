module Snake where

import Base
import Control.Monad.Eff
import Graphics.Canvas
import Data.Foldable
import Data.Array.NonEmpty
import Data.Maybe
import Control.Monad.Eff.Random
import Random (randomN)

type CanvasEff a = forall e. Eff (canvas :: Canvas | e) a

type Position = {x :: Number, y :: Number}

type KeyCode = Number

red = "rgb(200,10,10)"
black = "rgb(0,0,0)"

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
drawSnake ctx tr (Snake _ ps) = for_ (map tr ps) $ fillRectWithStyle ctx black

drawApple :: Context2D -> Number -> (Position -> Rectangle) -> Eff (canvas :: Canvas, random :: Random) Unit
drawApple ctx sz tr = do
    pos <- chooseApplePosition sz
    let rect = tr pos
    _ <- fillRectWithStyle ctx red rect
    return unit  

fillRectWithStyle :: forall e. Context2D -> String -> Rectangle -> Eff (canvas :: Canvas | e) Unit
fillRectWithStyle ctx style rect = do
    _ <- setFillStyle style ctx
    _ <- fillRect ctx rect
    return unit  

keyToDirection :: KeyCode -> Maybe Direction
keyToDirection k = 
    case k of
        37 -> Just W
        39 -> Just E
        40 -> Just S
        38 -> Just N
        _ -> Nothing

chooseApplePosition :: Number -> forall e. Eff (random :: Random | e) Position
chooseApplePosition sz = do
    x <- randomN sz
    y <- randomN sz
    return $ {x:x, y:y}

type ShowApple = Boolean

shouldShowApple :: forall e. Eff (random :: Random | e) ShowApple
shouldShowApple = (\x -> x < 0.05) <$> random

type Result = {snake :: Snake, crashed :: Boolean, showApple :: ShowApple}

mkLoop :: Eff (canvas :: Canvas) (KeyCode -> ShowApple -> Snake -> Eff (canvas :: Canvas, random :: Random) Result)
mkLoop = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    let board = {w: 800, h: 800, squares: 40}
    return $ loop board ctx

loop :: Board -> Context2D -> KeyCode -> ShowApple -> Snake -> Eff (canvas :: Canvas, random :: Random) Result
loop board ctx keyCode showApple s@(Snake d body) = 
    let d' = fromMaybe d (keyToDirection keyCode)
        s' = moveSnake d' s in 
        if isSnakeOutsideBoard board s' 
        then return $ {snake: s', crashed: true, showApple: false}
        else do
            _ <- clearRect ctx $ (toView board) (snakeTail s)
            _ <- drawSnake ctx (toView board) s'
            showApple' <- if showApple then return true else shouldShowApple
            _ <- if showApple' && (not showApple) then drawApple ctx board.squares (toView board) else (return unit)
            return $ {snake: s', crashed: false, showApple: showApple'}
