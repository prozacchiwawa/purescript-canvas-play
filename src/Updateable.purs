module Updateable where

import Prelude
import Data.DateTime.Instant
import Effect (Effect)
import Graphics.Canvas as C

import GameInputs
    
class Updateable a where
    update :: a -> Instant -> GameInputs -> a
                   
