
module Test.Test
  ( testAll
  ) where

import Core.Body
import Data.Maybe
import Data.Ratio
import Control.Monad
import System.IO
import Base.Openess
import Util.Pretty

anchors = [ (0,0), (2,0), (2,2), (0,2), (-2,2), (-2,0), (-2,-2), (0,-2), (2,-2) ]
bodyTypes = [ Open, Closed ]
velocities = [ (-1,-1), (0,-1), (1,-1), (-1,0), (0,0), (1,0), (-1,1), (0,1), (1,1) ]
testBody velocity bodyType (x,y) = setBodyVelocity velocity (fromJust $ newBody bodyType (x-1) (y-1) (x+1) (y+1))

printVel (vx,vy) = putStrLn . concat $ ["vel: ", "(",show . numerator $ vx, ",", show . numerator $ vy, ")" ]

tests :: [IO ()]
tests = do
  anchor <- anchors
  bodyType <- bodyTypes
  vel <- velocities
  let myBody = testBody vel bodyType anchor
  return $ do
    putStrLn . prettyAABB . shape_ $ myBody
    printVel vel
    print $ getBodyCollisionTimeToZero myBody
    x <- getChar
    putStrLn [x]


testAll :: IO ()
testAll = sequence_ tests
