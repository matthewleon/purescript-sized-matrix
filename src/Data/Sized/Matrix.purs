module Data.Sized.Matrix where

import Prelude
import Data.Bifunctor (bimap)
import Data.Distributive (distribute)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num.Ops (class Lt)
import Data.Typelevel.Num.Reps (D0, D1)
import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (Vec)
import Data.Vec as V

newtype Matrix m n a = Matrix (Vec m (Vec n a))
derive instance newtypeMatrix :: Newtype (Matrix m n a) _

empty :: forall a. Matrix D0 D0 a
empty = wrap V.empty

singleton :: forall a. a -> Matrix D1 D1 a
singleton = replicate

replicate :: forall m n a. Nat m => Nat n => a -> Matrix m n a
replicate = wrap <<< V.replicate' <<< V.replicate'

rows :: forall m n a. Matrix m n a -> Vec m (Vec n a)
rows = unwrap

row :: forall m n a i. Nat m => Nat i => Lt i m => Matrix m n a -> i -> Vec n a
row m _ =  V.index (rows m) (undefined :: i)

cols :: forall m n a. Nat m => Nat n => Matrix m n a -> Vec n (Vec m a)
cols = distribute <<< rows

col :: forall m n a j. Nat m => Nat n => Nat j => Lt j n => Matrix m n a -> j -> Vec m a
col m _ = map (\v -> V.index v (undefined :: j)) $ rows m

index :: forall m n a i j. Nat m => Nat n => Nat i => Nat j => Lt i m => Lt j n
      => Matrix m n a -> i -> j -> a
index m _ _ = V.index (row m (undefined :: i)) (undefined :: j)

index' :: forall m n a. Matrix m n a -> Int -> Int -> Maybe a
index' m i j = V.index' (rows m) i >>= \v -> V.index' v j

transpose :: forall m n a. Nat m => Nat n => Matrix m n a -> Matrix n m a
transpose = wrap <<< cols

dims :: forall m n a. Nat m => Nat n => Matrix m n a -> Tuple m n
dims _ = Tuple undefined undefined

dims' :: forall m n a. Nat m => Nat n => Matrix m n a -> Tuple Int Int
dims' = bimap toInt toInt <<< dims
