
module Core.Body
  ( Body(..)
  , setVelocity
  , updateBody
  ) where

import Base.Time
  ( Time(..)
  )

import Base.Openess
  ( Openess
  )

import Base.Coordinate
  ( Coordinate
  )

import Base.Vector
  ( Vector
  , sumVectors
  )

import Primitive.AABB
  ( AABB
  , newAABB
  , translateAABB
  )


data Body = Body
  { velocity_ :: Vector
  , shape_    :: AABB
  } deriving (Ord, Eq, Show)


newBody :: Openess -> Coordinate -> Coordinate
                   -> Coordinate -> Coordinate -> Maybe Body
newBody openess left top right bottom = do
  aabb <- newAABB openess left top right bottom
  return $ Body (0,0) aabb

setVelocity :: Vector -> Body -> Body
setVelocity vector (Body velocity shape) =
  Body (velocity `sumVectors` vector) shape

updateBody :: Body -> Body
updateBody (Body velocity shape) =
  Body velocity $ translateAABB velocity shape

