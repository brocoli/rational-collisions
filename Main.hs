
--import Control.Concurrent
--  ( threadDelay
--  )

--import Data.Maybe
--  ( fromJust
--  )
--import Base.Openess
--  ( Openess(..)
--  )
--import Core.Body
--  ( Body(..)
--  , newBody
--  , setBodyVelocity
--  , stepBody
--  )

--import Util.Pretty
--  ( prettyAABB
--  )

------ =========
------ = Debug =
------ =========
----import Debug.Trace
----  ( traceShow
----  )
----traceShowThis x = traceShow x x

---- ===========
---- = Control =
---- ===========
---- recursive call with recursive state and the possibility of exit.
--repeatEither :: Monad m => (s -> m (Either e s)) -> (Either e s) -> m e
--repeatEither f = either return $ (repeatEither f =<<) . f

---- repeatEither with initial value.
--repeatEither1 :: Monad m => (s -> m (Either e s)) -> s -> m e
--repeatEither1 f = repeatEither f . return


---- ==========
---- = Engine =
---- ==========
--data EState = EState
--  { wall_    :: Body
--  , critter_ :: Body
--  } deriving (Ord,Eq,Show)

--stepEngine :: EState -> IO (Either () EState)
--stepEngine (EState wall critter) = do
--  putStrLn . prettyAABB . shape_ $ critter
--  putStrLn . prettyAABB . shape_ $ wall
--  putStrLn ""
--  threadDelay 50000
--  return . Right . EState wall $ stepBody critter

--initialState :: EState
--initialState = EState
--  { wall_    = fromJust $ newBody Open 9 (-1) 10 1
--  , critter_ = setBodyVelocity (1,0) (fromJust $ newBody Open (-1) (-1) 1 1)
--  }

--runEngine :: IO ()
--runEngine = repeatEither1 stepEngine initialState

---- ========
---- = Main =
---- ========
--main = runEngine

import Test.Test
  ( testAll
  )

main = testAll
