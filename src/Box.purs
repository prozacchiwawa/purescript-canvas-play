module Box where

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.List as List
import Effect (Effect)
import Graphics.Canvas as C
        
import GameState
import Renderable
import Scene

data Box = Box
    { x :: Number
    , speed :: Number
    , npc :: Boolean
    , color :: String
    , hit :: Boolean
    }

initialBox :: Number -> Number -> Boolean -> String -> Box
initialBox x speed npc color =
    Box
    { x: x 
    , speed: speed
    , npc: npc
    , color: color
    , hit: false
    }

drawRect :: C.Context2D -> Box -> Effect Unit
drawRect ctx (Box state) = do
  _ <- C.setFillStyle ctx state.color
  _ <- C.fillRect ctx
       { x: state.x
       , y: scene.height / 2.0
       , w: scene.boxSize
       , h: scene.boxSize
       }
  pure unit

instance renderableBox :: Renderable Box where
    render b@(Box box) pass context =
        case pass of
          Characters -> do
            drawRect context b
          _ -> do
            pure unit

instance updatedableBox :: UpdateInState Box where
    updateInState b@(Box box) s@(State state) =
        let
            elapsed = gameTime s
        
            reverse = 
                box.x + scene.boxSize > scene.width ||
                   box.x < scene.x ||
                      (box.npc && (not box.hit) && state.jump)
        in
        if reverse then
            Tuple
            (Box
             (box
              { x =
                    if box.x < scene.x then
                        scene.x
                    else if box.x + scene.boxSize > scene.width then
                        scene.width - scene.boxSize
                    else
                        box.x
                                
              , speed = -box.speed
              , hit = state.jump
              }
             )
            )
            List.Nil
        else
            Tuple
            (Box
             (box
              { x = box.x + (box.speed * 60.0 * elapsed)
              , hit = state.jump
              }
             )
            )
            List.Nil
      
