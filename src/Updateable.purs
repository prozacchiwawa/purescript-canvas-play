module Updateable where

import Prelude
import Data.DateTime.Instant
import Data.Tuple (Tuple(..))
import Data.List (List)
import Effect (Effect)
import Graphics.Canvas as C

import GameInputs
    
class Updateable a where
    update :: a -> GameEvent -> Tuple a (List GameEffect)
