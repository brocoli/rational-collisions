
module Core.Body
  ( Body(..)
  ) where

import Base.Vector
  ( Vector
  )

import Primitive.AABB
  ( AABB
  , newAABB
  )


data Body = Body
  { shape_    :: AABB
  , velocity_ :: Vector
  } deriving(Ord,Eq,Show)


