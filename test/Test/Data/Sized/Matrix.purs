module Test.Data.Sized.Matrix (testMatrix) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Data.Typelevel.Num.Reps (D1, D2, D3)
import Data.Sized.Matrix (Matrix, replicate)

testMatrix :: forall e. Eff (console :: CONSOLE | e) Unit
testMatrix = do
  -- for now, visually inspect these
  logShow (replicate 'o' :: Matrix D3 D1 Char)
  logShow (replicate 'o' :: Matrix D1 D3 Char)
  logShow (replicate 'o' :: Matrix D3 D2 Char)
  logShow (replicate 'o' :: Matrix D2 D3 Char)
