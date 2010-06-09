module SDLUtils ( loadImage, setColorKey, rectBoundaries, doRectsCollide,
                  getPixel )
       where

import Data.Word (Word8)
import Foreign (peekElemOff)
import Unsafe.Coerce (unsafeCoerce)


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

rectBoundaries :: SDL.Rect -> (Int, Int, Int, Int)
rectBoundaries (SDL.Rect x y w h) = (left, right, top, bottom)
  where left   = x
        right  = x + w
        top    = y
        bottom = y + h

doRectsCollide :: SDL.Rect -> SDL.Rect -> Bool
doRectsCollide a b = not $ or [ aleft >= bright, aright <= bleft
                              , atop >= bbottom, abottom <= btop ]
  where (aleft, aright, atop, abottom) = rectBoundaries a
        (bleft, bright, btop, bbottom) = rectBoundaries b

getPixel :: SDL.Surface -> Int -> Int -> IO SDL.Pixel
getPixel surface x y = do
  let width  = SDL.surfaceGetWidth surface
  let offset = y * width + x

  pixels <- fmap unsafeCoerce $ SDL.surfaceGetPixels surface
  fmap SDL.Pixel $ peekElemOff pixels offset
