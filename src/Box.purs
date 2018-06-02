module Box where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C
import Data.DateTime.Instant
import Data.Time.Duration
import GameInputs
import Renderable
import Scene
import Updateable
        
data State = State
    { x :: Number
    , step :: Number
    , s :: Instant
    , t :: Instant
    }

initialState :: Instant -> State
initialState inst = State
    { x: 0.0
    , step: 10.0
    , s: inst
    , t: inst
    }

drawRect :: forall e. C.Context2D -> State -> Effect Unit
drawRect ctx (State state) = do
  _ <- C.setFillStyle ctx "#0088DD"
  _ <- C.fillRect ctx
       { x: state.x
       , y: scene.height / 2.0
       , w: scene.boxSize
       , h: scene.boxSize
       }
  pure unit

updateState :: State -> Instant -> State
updateState (State state) inst =
    if state.x + scene.boxSize > scene.width then
        State
        { x: scene.width - scene.boxSize
        , step: -state.step
        , t: inst
        , s: state.s
        }
  else if state.x < scene.x then
           State
           { x: scene.x
           , step: -state.step
           , t: inst
           , s: state.s
           }
       else
           State
           { x: state.x + state.step
           , step: state.step
           , t: inst
           , s: state.s
           }
              
instance renderableState :: Renderable State where
    render s@(State state) context = do
      let (Milliseconds uit) = unInstant state.t
      let (Milliseconds uis) = unInstant state.s
      drawRect context s
      C.fillText context (show (uit - uis)) 10.0 10.0

instance updatedableState :: Updateable State where
    update s t inputs = updateState s t
