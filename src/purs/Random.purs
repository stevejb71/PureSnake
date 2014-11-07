module Random where

import Math (floor)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random

randomN :: Number -> forall e. Eff (random :: Random | e) Number
randomN n = (\x -> floor(x * n)) <$> random