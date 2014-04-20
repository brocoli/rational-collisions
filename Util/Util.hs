
module Util.Util
  ( mapTuple
  ) where

import Control.Monad
  ( join
  )

import Control.Arrow
  ( (***)
  )


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)
