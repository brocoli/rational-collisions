
module Util.Util
  ( mapTuple
  , (>|<)
  , parallelAp
  , swapCross
  , (.:)
  ) where

import Control.Monad
  ( join
  )
import Control.Arrow
  ( (***)
  )

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

(>|<) :: (a -> b -> c) -> (d -> e -> f) -> (a,d) -> (b,e) -> (c,f)
(f >|< g) (x,y) (z,w) = (f x z, g y w)

parallelAp :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
parallelAp = join (>|<)

swapCross :: ((a,b),(c,d)) -> ((a,c),(b,d))
swapCross ((x,y),(z,w)) = ((x,z),(y,w))

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
