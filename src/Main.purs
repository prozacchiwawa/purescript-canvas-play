-- https://gist.github.com/soupi/76103c45fc00a6c1478e
module Main where

import Prelude

import Effect (Effect)
--import Effect.Console (log)
import Effect.Now (now)
import Data.Maybe (Maybe(Just, Nothing))
--import Data.DateTime.Instant
--import Web.DOM
import Graphics.Canvas as C
import Signal (foldp, runSignal, unwrap, merge)
import Signal.DOM (animationFrame, keyPressed)

import Box as Box
import GameInputs (GameEvent(..))
import Renderable (render)
import Scene (scene)
import Updateable (update)

main :: Effect Unit
main = do
  mcanvas <- C.getCanvasElementById "scene"
  case mcanvas of
         Just canvas -> do
                 context <- C.getContext2D canvas
                 frames <- animationFrame
                 currentInstant <- now
                 jumpSignal <- keyPressed 32
                 timeSignal <- unwrap (map (\_ -> now) frames)

                 let game =
                         foldp
                             updateGame
                             (Box.initialState currentInstant)
                             (merge
                              (map JumpPressed jumpSignal)
                              (map Time timeSignal)
                             )
                 runSignal (renderGame context <$> game)
         Nothing -> do
                 pure unit

updateGame :: GameEvent -> Box.State -> Box.State
updateGame event state = update state event
                  
renderGame :: C.Context2D -> Box.State -> Effect Unit
renderGame context state = do
  clearCanvas context
  render state context
  pure unit

clearCanvas :: C.Context2D -> Effect Unit
clearCanvas ctx = do
  _ <- C.setFillStyle ctx "#000000"
  _ <- C.fillRect ctx { x: 0.0, y: 0.0, w: scene.width, h: scene.height }
  pure unit
