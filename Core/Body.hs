
module Core.Body
  ( Body(..)
  , newBody
  , setBodyVelocity
  , updateBody
  , stepBody
  ) where

import Base.Time
  ( Time
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
  } deriving (Ord,Eq,Show)


newBody :: Openess -> Coordinate -> Coordinate
                   -> Coordinate -> Coordinate -> Maybe Body
newBody openess left top right bottom = do
  aabb <- newAABB openess left top right bottom
  return $ Body (0,0) aabb

setBodyVelocity :: Vector -> Body -> Body
setBodyVelocity vector (Body velocity shape) =
  Body (velocity `sumVectors` vector) shape

updateBody :: Time -> Body -> Body
updateBody time (Body (vx,vy) shape) =
  Body (vx,vy) $ translateAABB (time*vx,time*vy) shape

stepBody :: Body -> Body
stepBody = updateBody 1
