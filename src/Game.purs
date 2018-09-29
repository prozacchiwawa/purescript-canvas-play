module Game where

import Prelude

import Effect (Effect)
--import Effect.Console (log)
import Data.DateTime.Instant
import Graphics.Canvas as C
import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable as Traversable
        
import Box (Box)
import Box as Box

import GameInputs
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
                (BoxEnt (Box.initialBox 0.0 10.0 true "blue"))
                (List.Cons
                         (BoxEnt (Box.initialBox 30.0 12.0 false "red"))
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
          BoxEnt b ->
              let (Tuple be effs) = updateInState b state in
              Tuple (BoxEnt be) effs

wantsToDie effs = not (List.null (List.filter ((==) Die) effs))

runCreateEffect :: Game -> List CreateArgument -> Game
runCreateEffect game args = game
    
runEffects :: Game -> List GameEffect -> Game
runEffects game effs =
    Traversable.foldl
      (\(Game state) eff ->
           case eff of
             CreateEntity args -> runCreateEffect game args
             _ -> game
      )
      game
      effs

instance gameUpdateable :: Updateable Game where
    update (Game game) evt =
        let
            (Tuple newState effects) = update game.state evt

            newEntitiesWithEffects :: List (Tuple Entity (List GameEffect))
            newEntitiesWithEffects =
                map (\e -> updateInState e newState) game.entities
       
            participantEffects =
                List.concat (map (\(Tuple _ eff) -> eff) newEntitiesWithEffects)

            survivors =
                map (\(Tuple ent _) -> ent)
                  (List.filter
                   (\(Tuple _ eff) -> not (wantsToDie eff)) newEntitiesWithEffects
                  )
        in
        Tuple
        (Game
         { state: newState
         , entities: survivors
         }
        )
        (List.concat (List.Cons effects (List.Cons participantEffects List.Nil)))

instance gameRenderable :: Renderable Game where
    render (Game game) pass context =
        let
            renderAll pass entities =
                case entities of
                  List.Cons hd tl -> do
                    render hd pass context
                    renderAll pass tl
                  List.Nil -> do
                    pure unit
        in do
          render game.state pass context
          renderAll pass game.entities
