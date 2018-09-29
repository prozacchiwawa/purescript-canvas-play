module GameInputs where

import Prelude
import Data.DateTime.Instant
import Data.List (List)
        
data GameEvent
    = JumpPressed Boolean
    | Time Instant

derive instance eqGameEvent :: Eq GameEvent

data CreateArgument
    = StringArg String
    | NumberArg Number

derive instance eqCreateArgument :: Eq CreateArgument
      
data GameEffect
    = CreateEntity (List CreateArgument)
    | Die

derive instance eqGameEffect :: Eq GameEffect

data GameInputs = GameInputs
    { jump :: Boolean
    }
