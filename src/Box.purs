module Box where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.String as DS
import Data.String.CodeUnits as SCU
import Data.Foldable as Foldable
import Data.Tuple (Tuple(..))

import GameState
import Renderable
import Scene
import SHA1 as SHA1

data Box = Box
    { x :: Number
    , color :: String
    , seed :: Number
    , oldSizes :: Array Number
    , newSizes :: Array Number
    , sizes :: Array Number
    }

charToSize :: Char -> Number
charToSize ch =
    case ch of
      '0' -> 0.3
      '1' -> 0.5
      '2' -> 0.7
      '3' -> 0.9
      '4' -> 1.3
      '5' -> 1.7
      '6' -> 2.1
      '7' -> 2.2
      '8' -> 2.3
      '9' -> 2.4
      'a' -> 2.5
      'b' -> 2.7
      'c' -> 2.9
      'd' -> 3.0
      'e' -> 3.1
      _   -> 3.2
             
sizesFromSeed :: Number -> Array Number
sizesFromSeed n =
    let str = SHA1.doSha1 (show n) in
    map charToSize (SCU.toCharArray str)

initialBox :: String -> Box
initialBox boxcolor =
    Box
    { x: 0.0
    , color: boxcolor
    , seed: 1.0
    , sizes: sizesFromSeed 1.0
    , newSizes: sizesFromSeed 1.0
    , oldSizes: sizesFromSeed 0.0
    }

drawRect :: C.Context2D -> Box -> Effect Unit
drawRect ctx (Box state) = do
  let x = 10.0
  let y = scene.height * 5.0 / 8.0
  let height = scene.height / 4.0
  _ <- C.setFont ctx ((show height) <> "px Arial")
  _ <- C.setFillStyle ctx state.color
  totalWidth <- Foldable.foldM
       (\x (Tuple xscale ch) -> do
          let s = SCU.singleton ch
          m <- C.measureText ctx s
          _ <- C.scale ctx { scaleX: xscale, scaleY: 1.0 }
          _ <- C.scale ctx { scaleX: 1.0 / xscale, scaleY: 1.0 }
          pure (x + m.width * xscale)
       )
       0.0
       (Array.zip state.sizes (SCU.toCharArray "Monad"))
  _ <- Foldable.foldM
       (\x (Tuple xscale ch) -> do
          let s = SCU.singleton ch
          m <- C.measureText ctx s
          let targetWidth = scene.width * 3.0 / 4.0
          let idealScale = targetWidth / totalWidth
          let wantScale = idealScale * xscale
          _ <- C.scale ctx { scaleX: wantScale, scaleY: 1.0 }
          _ <- C.fillText ctx s (x / wantScale) y
          _ <- C.scale ctx { scaleX: 1.0 / wantScale, scaleY: 1.0 }
          pure (x + m.width * wantScale)
       )
       ((scene.width - totalWidth) / 8.0)
       (Array.zip state.sizes (SCU.toCharArray "Monad"))
  pure unit

instance renderableBox :: Renderable Box where
    render b@(Box box) context =
        drawRect context b

instance updatedableBox :: UpdateInState Box where
    updateInState b@(Box box) s@(State state) =
        let
            elapsed = gameTime s
        
            reverse = box.x >= 1.0
        in
        if reverse then
            Box
            (box
             { x = 0.0
             , seed = box.seed + 1.0
             , newSizes = sizesFromSeed box.seed
             , oldSizes = box.sizes
             }
            )
        else
            let
                newArray =
                  map
                           (\(Tuple n o) -> n * box.x + (o * (1.0 - box.x)))
                           (Array.zip
                                 box.newSizes
                                 box.oldSizes
                           )

                newTotalSize = Foldable.foldl (+) 0.0 newArray
            in
            (Box
             (box
              { x = box.x + elapsed
              , sizes = map (\s -> s / newTotalSize) newArray
              }
             )
            )
      
