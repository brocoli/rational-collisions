
module Base.Vector
  ( Vector(..)
  , sumVectors
  , subtractVectors
  ) where

import Util.Util
  ( parallelAp
  )
import Base.Coordinate
  ( Coordinate
  )


type Vector = (Coordinate,Coordinate)


sumVectors :: Vector -> Vector -> Vector
sumVectors = parallelAp (+)

subtractVectors :: Vector -> Vector -> Vector
subtractVectors = parallelAp (-)
