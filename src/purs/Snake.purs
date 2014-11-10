module Snake where

import Base
import Control.Monad.Eff (Eff())
import Graphics.Canvas (Canvas(), Rectangle(), getCanvasElementById, getContext2D, Context2D(), clearRect)
import Data.Array.NonEmpty
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import Data.Maybe.Unsafe (fromJust)
import Control.Monad.Eff.Random (Random(), random)
import Random (randomN)
import Drawing

type KeyCode = Number

type Board = {w :: Number, h :: Number, squares :: Number}

snakeTail :: Snake -> Position
snakeTail (Snake _ body) = last body

snakeHead :: Snake -> Position
snakeHead (Snake _ (NonEmpty h _)) = h

direction (Snake d _) = d

isSnakeOutsideBoard :: Board -> Snake -> Boolean
isSnakeOutsideBoard b (Snake _ (NonEmpty {x=x,y=y} _)) = x < 0 || y < 0 || x >= b.squares || y >= b.squares

starterSnake :: Snake
starterSnake = Snake S ({x:5,y:5} :| [{x:6,y:5}])

changeDirection :: Direction -> Snake -> Snake
changeDirection d' (Snake _ ps) = Snake d' ps

moveSnake :: Direction -> Boolean -> Snake -> Snake
moveSnake d grow (Snake _ body) = 
    let p = head body
        p' = case d of 
                N -> {x:p.x, y:p.y-1}
                S -> {x:p.x, y:p.y+1}
                W -> {x:p.x-1, y:p.y}
                E -> {x:p.x+1, y:p.y}
        body' = if grow then p' <| body else p' :| pop body
    in Snake d body' 

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

nextApplePosition :: forall e. Number -> Eff (random :: Random | e) (Maybe ApplePosition)
nextApplePosition sz = do
    showApple <- (\x -> x < 0.1) <$> random
    pos <- if showApple then Just <$> (choosePosition sz) else return noApple
    return pos
    where choosePosition sz = do
            x <- randomN sz
            y <- randomN sz
            return {x:x, y:y}

snakeHasEatenApple :: Maybe ApplePosition -> Snake -> Boolean
snakeHasEatenApple a s = fromMaybe false $ eqPosition <$> a <*> Just (snakeHead s)

type Result = {snake :: Snake, crashed :: Boolean, appleAt :: Maybe ApplePosition, snakeHasEatenApple :: Boolean}

type Input = {keyCode :: KeyCode, snake :: Snake, appleAt :: Maybe ApplePosition, growSnake :: Boolean}

mkLoop :: Eff (canvas :: Canvas) (Input -> Eff (canvas :: Canvas, random :: Random) Result)
mkLoop = do 
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    let board = {w: 800, h: 800, squares: 40}
    return $ loop board ctx

noApple :: Maybe ApplePosition
noApple = Nothing

loop :: Board -> Context2D -> Input -> Eff (canvas :: Canvas, random :: Random) Result
loop board ctx input = 
    let s = input.snake
        d = direction s
        applePosition = input.appleAt
        d' = fromMaybe d (keyToDirection input.keyCode)
        s' = moveSnake d' input.growSnake s in 
    if isSnakeOutsideBoard board s' 
    then return {snake: s', crashed: true, appleAt: noApple, snakeHasEatenApple: false}
    else do
        let hasEaten = snakeHasEatenApple applePosition s'
        _ <- clearRect ctx $ (toView board) (snakeTail s)
        _ <- drawSnake ctx (toView board) s'
        applePosition' <- if isNothing applePosition 
                          then nextApplePosition board.squares 
                          else return $ if hasEaten then noApple else applePosition
        _ <- if isJust applePosition' && (isNothing applePosition) then drawApple ctx (fromJust applePosition') (toView board) else (return unit)
        return $ {snake: s', crashed: false, appleAt: applePosition', snakeHasEatenApple: hasEaten}
