module Box where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C
        
import GameState
import Renderable
import Scene

data Box = Box
    { x :: Number
    , speed :: Number
    }

initialBox :: Number -> Number -> Box
initialBox x speed =
    Box
    { x: x 
    , speed: speed
    }

drawRect :: C.Context2D -> Box -> Effect Unit
drawRect ctx (Box state) = do
  _ <- C.setFillStyle ctx "#0088DD"
  _ <- C.fillRect ctx
       { x: state.x
       , y: scene.height / 2.0
       , w: scene.boxSize
       , h: scene.boxSize
       }
  pure unit

instance renderableBox :: Renderable Box where
    render b@(Box box) context =
        drawRect context b

instance updatedableBox :: UpdateInState Box where
    updateInState b@(Box box) s@(State state) =
        let
            elapsed = gameTime s
        
            reverse = 
                box.x + scene.boxSize > scene.width ||
                   box.x < scene.x ||
                   state.jump
        in
        if reverse then
            Box
            (box
             { x =
                   if box.x < scene.x then
                       scene.x
                   else if box.x + scene.boxSize > scene.width then
                       scene.width - scene.boxSize
                   else
                       box.x
                        
             , speed = -box.speed
             }
            )
        else
            Box (box { x = box.x + (box.speed * 60.0 * elapsed) })
      
