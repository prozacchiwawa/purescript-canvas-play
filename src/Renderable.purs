module Renderable where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C

class Renderable a where
    render :: a -> C.Context2D -> Effect Unit

