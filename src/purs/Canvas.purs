module Canvas where

import Control.Monad.Eff

foreign import data CanvasIO :: !

foreign import data Context2D :: *

foreign import canvasContext_
    "function canvasContext_(name) {\
    \   var c = document.getElementById('canvas');\
    \   return c.getContext('2d');\
    \}" :: String -> Context2D
canvasContext :: String -> Eff (c :: CanvasIO) Context2D
canvasContext name = return $ canvasContext_ name

foreign import drawRect_
    "function drawRect_(ctx) {\
    \   ctx.fillStyle = '#FF0000';\
    \   ctx.fillRect(0,0,150,75);\
    \}" :: Context2D -> Unit
drawRect :: Context2D -> Eff (c :: CanvasIO) Unit
drawRect ctx = return $ drawRect_ ctx