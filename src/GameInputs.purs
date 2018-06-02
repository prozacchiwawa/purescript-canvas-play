module GameInputs where

import Prelude
import Data.DateTime.Instant
        
data GameEvent
    = JumpPressed Boolean
    | Time Instant
    
data GameInputs = GameInputs
    { jump :: Boolean
    }
