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
    , speed :: Number
    , s :: Instant
    , t :: Instant
    }

initialState :: Instant -> State
initialState inst = State
    { x: 0.0
    , speed: 10.0
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

gameTime (State state) =
    let
        (Milliseconds uit) = unInstant state.t
        (Milliseconds uis) = unInstant state.s
    in
    (uit - uis) / 1000.0
       
updateState :: State -> Instant -> State
updateState s@(State state_) inst =
    let
        su@(State state) =
            State
            (state_ { t = inst, s = state_.t })
            
        elapsed = gameTime su
    in
    if state.x + scene.boxSize > scene.width then
        State
        (state
         { x = scene.width - scene.boxSize
         , speed = -state.speed
         }
        )
  else if state.x < scene.x then
           State
           (state
            { x = scene.x
            , speed = -state.speed
            }
           )
       else
           State
           (state
            { x = state.x + (state.speed * 60.0 * elapsed)
            }
           )
              
instance renderableState :: Renderable State where
    render s@(State state) context = do
      drawRect context s
      C.fillText context (show (gameTime s)) 10.0 10.0

instance updatedableState :: Updateable State where
    update s t inputs = updateState s t
