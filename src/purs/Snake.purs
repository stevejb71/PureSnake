module Snake where

import Base
import Control.Monad.Eff (Eff())
import Graphics.Canvas (Canvas(), Rectangle(), getCanvasElementById, getContext2D, Context2D(), clearRect)
import Data.Array.NonEmpty
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Eff.Random (Random(), random)
import Random (randomN)
import Drawing

type KeyCode = Number

type Board = {w :: Number, h :: Number, squares :: Number}

snakeTail :: Snake -> Position
snakeTail (Snake _ body) = last body

isSnakeOutsideBoard :: Board -> Snake -> Boolean
isSnakeOutsideBoard b (Snake _ (NonEmpty {x=x,y=y} _)) = x < 0 || y < 0 || x >= b.squares || y >= b.squares

starterSnake :: Snake
starterSnake = Snake S ({x:5,y:5} :| [{x:6,y:5}])

changeDirection :: Direction -> Snake -> Snake
changeDirection d' (Snake _ ps) = Snake d' ps

moveSnake :: Direction -> Snake -> Snake
moveSnake d (Snake _ body) = 
    let p = head body
        p' = case d of 
                N -> {x:p.x, y:p.y-1}
                S -> {x:p.x, y:p.y+1}
                W -> {x:p.x-1, y:p.y}
                E -> {x:p.x+1, y:p.y}
    in Snake d (p' :| (pop body)) 

toView :: Board -> Position -> Rectangle
toView b p = { h: sqHeight, w: sqWidth, x: p.x * sqWidth, y: p.y * sqHeight }
    where sqWidth = b.w / b.squares 
          sqHeight = b.h / b.squares 

keyToDirection :: KeyCode -> Maybe Direction
keyToDirection k = 
    case k of
        37 -> Just W
        39 -> Just E
        40 -> Just S
        38 -> Just N
        _ -> Nothing

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
