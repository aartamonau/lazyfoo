module SDLUtils ( loadImage, setColorKey )
       where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage

setColorKey surface (r, g, b) = do
  let format =  SDL.surfaceGetPixelFormat surface
  colorkey   <- SDL.mapRGB format r g b

  SDL.setColorKey surface [SDL.SrcColorKey] colorkey

  return surface
