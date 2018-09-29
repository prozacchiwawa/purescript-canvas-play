-- https://gist.github.com/soupi/76103c45fc00a6c1478e
module Main where

import Prelude

import Effect (Effect)
--import Effect.Console (log)
import Effect.Now (now)
import Data.DateTime.Instant
import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable as Traversable
--import Data.DateTime.Instant
--import Web.DOM
import Graphics.Canvas as C
import Signal (foldp, runSignal, unwrap, merge)
import Signal.DOM (animationFrame, keyPressed)

import Box (Box)
import Box as Box
import Game
import GameInputs (GameEvent(..))
import GameState as GameState
import GameState
import Renderable
import Scene (scene)
import Updateable

clearCanvas :: C.Context2D -> Effect Unit
clearCanvas ctx = do
  _ <- C.setFillStyle ctx "#000000"
  _ <- C.fillRect ctx { x: 0.0, y: 0.0, w: scene.width, h: scene.height }
  pure unit

updateGame :: GameEvent -> Game -> Game
updateGame event state =
    let (Tuple newState effects) = update state event in
    runEffects newState effects

renderGame :: C.Context2D -> Game -> Effect Unit
renderGame context state = do
  clearCanvas context
  render state Background context
  render state Moveables context
  render state Characters context
  render state Flair context
  render state Hud context
  pure unit

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
                             (init currentInstant)
                             (merge
                              (map JumpPressed jumpSignal)
                              (map Time timeSignal)
                             )
                 runSignal (renderGame context <$> game)
         Nothing -> do
                 pure unit
