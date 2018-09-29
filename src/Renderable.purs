module Renderable where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C

data RenderPass
    = Background
    | Moveables
    | Characters
    | Flair
    | Hud
    
class Renderable a where
    render :: a -> RenderPass -> C.Context2D -> Effect Unit
