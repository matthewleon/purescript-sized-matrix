module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Unit (Unit)

import Test.Data.Sized.Matrix (testMatrix)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = testMatrix
