module GameState where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C
import Data.DateTime.Instant
import Data.List (List)
import Data.Time.Duration
import GameInputs
import Renderable
import Scene
import Updateable
        
data State = State
    { s :: Instant
    , t :: Instant
    , o :: Instant
    , jump :: Boolean
    }

class UpdateInState a where
    updateInState :: a -> State -> a
             
initialState :: Instant -> State
initialState inst = State
    { s: inst
    , t: inst
    , o: inst
    , jump: false
    }

gameTime (State state) =
    let
        (Milliseconds uit) = unInstant state.t
        (Milliseconds uis) = unInstant state.s
    in
    (uit - uis) / 1000.0
       
instance renderableState :: Renderable State where
    render s@(State state) context = do
      C.fillText context (show (gameTime s)) 10.0 10.0

instance updatedableState :: Updateable State where
    update s@(State state) evt =
        case evt of
          Time i -> State (state { t = i, s = state.t })
          JumpPressed j -> State (state { jump = j })
