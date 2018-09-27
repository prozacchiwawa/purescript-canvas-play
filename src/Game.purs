module Game where

import Prelude

import Effect (Effect)
--import Effect.Console (log)
import Data.DateTime.Instant
import Graphics.Canvas as C
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(Just, Nothing))

import Box (Box)
import Box as Box

import GameInputs (GameEvent(..))
import GameState as GameState
import GameState
import Renderable
import Scene (scene)
import Updateable

data Entity
    = BoxEnt Box 
    
data Game = Game
    { state :: State
    , entities :: List Entity
    }

init :: Instant -> Game
init inst =
    Game
    { state: GameState.initialState inst
    , entities:
        List.Cons
                (BoxEnt (Box.initialBox "#687a99"))
                (List.Cons
                         (BoxEnt (Box.initialBox "#cbd1db"))
                         List.Nil
                )
    }

instance renderEntity :: Renderable Entity where
    render e context =
        case e of
          BoxEnt b -> do
                 render b context

instance updateEntity :: UpdateInState Entity where
    updateInState e state =
        case e of
          BoxEnt b -> BoxEnt (updateInState b state)

instance gameUpdateable :: Updateable Game where
    update (Game game) evt =
        let newState = update game.state evt in
        Game
        { state: newState
        , entities: map (\e -> updateInState e newState) game.entities
        }

instance gameRenderable :: Renderable Game where
    render (Game game) context =
        let
            renderAll entities =
                case entities of
                  List.Cons hd tl -> do
                    render hd context
                    renderAll tl
                  List.Nil -> do
                    pure unit
        in do
          render game.state context
          renderAll game.entities
