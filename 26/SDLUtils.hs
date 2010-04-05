module SDLUtils ( loadImage, setColorKey )
       where

import Data.Word (Word8)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

loadImage :: FilePath -> IO SDL.Surface
loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage

setColorKey :: SDL.Surface -> (Word8, Word8, Word8) -> IO SDL.Surface
setColorKey surface (r, g, b) = do
  let format =  SDL.surfaceGetPixelFormat surface
  colorkey   <- SDL.mapRGB format r g b

  SDL.setColorKey surface [SDL.SrcColorKey] colorkey

  return surface
