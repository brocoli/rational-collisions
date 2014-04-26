
module Core.Body
  ( Body(..)
  , newBody
  , setBodyVelocity
  , updateBody
  , stepBody
  , getBodyCollisionTimeToZero
  ) where

import Control.Arrow
  ( (***)
  )
import Control.Monad
  ( join
  )
import Data.Maybe
  ( fromJust
  )
import Util.Util
  ( parallelAp
  , swapCross
  )
import Base.Time
  ( Time(..)
  , TimeFraction
  )
import Base.Openess
  ( Openess(..)
  )
import Base.Coordinate
  ( Coordinate
  )
import Base.Vector
  ( Vector
  , sumVectors
  )
import Base.IntervalWall
  ( IntervalWall(..)
  )
import Base.Interval
  ( Interval
  , hasZero
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

updateBody :: TimeFraction -> Body -> Body
updateBody fraction (Body (vx,vy) shape) =
  Body (vx,vy) $ translateAABB (fraction*vx,fraction*vy) shape

stepBody :: Body -> Body
stepBody = updateBody 1


-- Here are a bunch of building blocks for getBodyCollisionTimeToZero
solveTimeToZero :: Bool -> Bool -> Coordinate -> IntervalWall -> Time
solveTimeToZero leads containsZero vel (IntervalWall lean pos) =
  if vel == 0
    then
      if leads /= containsZero
        then Infinity
        else MinusInfinity
    else
      Finite $ (-pos)/vel

type TimeInterval = (Time,Time)

-- Interval and Coordinate arguments are flipped here in order to
-- compose better with the hasZero function. We flip them back
-- in getIntervalTimesToZero with the flip function
getTimesToZero :: Bool -> Interval -> Coordinate -> TimeInterval
getTimesToZero containsZero interval vel =
  let (leading,trailing) = if vel < 0
                             then (fst,snd)
                             else (snd,fst) in
    (solveTimeToZero True  containsZero vel $ leading  interval,
     solveTimeToZero False containsZero vel $ trailing interval)

getIntervalTimesToZero :: Coordinate -> Interval -> TimeInterval
getIntervalTimesToZero = flip . join $ getTimesToZero . hasZero

getAABBTimesToZero :: Vector -> AABB -> (TimeInterval,TimeInterval)
getAABBTimesToZero = parallelAp getIntervalTimesToZero

getBodyTimesToZero :: Body -> (TimeInterval,TimeInterval)
getBodyTimesToZero (Body velocity shape) = getAABBTimesToZero velocity shape

get2DCollisionToZeroInterval :: Body -> TimeInterval
get2DCollisionToZeroInterval =
  (uncurry max *** uncurry min) . swapCross . getBodyTimesToZero

getLeadingCornerOpeness :: Body -> Maybe Openess
getLeadingCornerOpeness
  ( Body (vx,vy)
    ((IntervalWall smallXLean _,IntervalWall bigXLean _),
     (IntervalWall smallYLean _,IntervalWall bigYLean _)) ) =
  do
    isXClosed <- case compare vx 0 of
                   LT -> Just $ smallXLean == EQ
                   EQ -> Nothing
                   GT -> Just $ bigXLean == EQ
    isYClosed <- case compare vy 0 of
                   LT -> Just $ smallYLean == EQ
                   EQ -> Nothing
                   GT -> Just $ bigYLean == EQ
    return $ if isXClosed && isYClosed
               then Closed
               else Open

getBodyCollisionTimeToZero :: Body -> Maybe Time
getBodyCollisionTimeToZero body =
  let (leading,trailing) = get2DCollisionToZeroInterval body in
    case compare leading trailing of
      LT -> Just leading
      EQ -> case fromJust $ getLeadingCornerOpeness body of
              Open   -> Nothing
              Closed -> Just leading
      GT -> Nothing
