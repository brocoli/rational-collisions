
module Util.Util
  ( mapTuple
  , (>-<)
  , parallelAp
  ) where

import Control.Monad
  ( join
  )

import Control.Arrow
  ( (***)
  )

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

(>-<) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
(f >-< g) (x,y) (z,w) = (f x z, g y w)

parallelAp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
parallelAp = join (>-<)
