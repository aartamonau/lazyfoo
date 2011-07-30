module SDLUtils ( loadImage, setColorKey, rectBoundaries, doRectsCollide,
                  getPixel, flipSurface, FlipFlag (..) )
       where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket_)
import Control.Monad (forM_)
import Data.Word ( Word8, Word32 )
import Foreign ( peekElemOff, pokeElemOff, withForeignPtr, peekByteOff )
import Unsafe.Coerce ( unsafeCoerce )


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

putPixel :: SDL.Surface -> Int -> Int -> SDL.Pixel -> IO ()
putPixel surface x y (SDL.Pixel color) = do
  let width  = SDL.surfaceGetWidth surface
  let offset = y * width + x

  pixels <- fmap unsafeCoerce $ SDL.surfaceGetPixels surface
  pokeElemOff pixels offset color

----------- haskell-sdl lacks this functions; completely unportable -----------
pixelFormatGetRMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetRMask = pixelFormatGetMask 20

pixelFormatGetGMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetGMask = pixelFormatGetMask 24

pixelFormatGetBMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetBMask = pixelFormatGetMask 28

pixelFormatGetAMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetAMask = pixelFormatGetMask 32

pixelFormatGetMask :: Int -> SDL.PixelFormat -> IO Word32
pixelFormatGetMask off format =
  withForeignPtr format $
    \ptr -> peekByteOff ptr off
-------------------------------------------------------------------------------

withSurface :: SDL.Surface -> IO a -> IO a
withSurface surface action =
  bracket_ (SDL.lockSurface surface) (SDL.unlockSurface surface) action

data FlipFlag = FlipVertical
              | FlipHorizontal
              deriving Eq

flipSurface :: SDL.Surface -> [FlipFlag] -> IO SDL.Surface
flipSurface surface [] = return surface
flipSurface surface flags = do
  surfaceFlags <- SDL.surfaceGetFlags surface
  let surfaceColorKeyed = SDL.SrcColorKey `elem` surfaceFlags

  let width  = SDL.surfaceGetWidth surface
  let height = SDL.surfaceGetHeight surface

  bpp <- fmap fromIntegral $ SDL.pixelFormatGetBitsPerPixel pixelFormat

  rMask <- pixelFormatGetRMask pixelFormat
  gMask <- pixelFormatGetGMask pixelFormat
  bMask <- pixelFormatGetBMask pixelFormat

  aMask <- if surfaceColorKeyed
             then return 0
             else pixelFormatGetAMask pixelFormat

  flipped <- SDL.createRGBSurface [SDL.SWSurface]
                                  width height bpp rMask gMask bMask aMask

  let flipV (x, y) = (x, height - 1 - y)
  let flipH (x, y) = (width - 1 - x, y)
  let flipBoth     = flipH . flipV

  let pixelMapFn | FlipVertical `elem` flags &&
                   FlipHorizontal `elem` flags = flipBoth
                 | FlipVertical `elem` flags   = flipV
                 | FlipHorizontal `elem` flags = flipH

  withSurface surface $ do
    forM_ [(x, y) | x <- [0 .. width - 1]
                  , y <- [0 .. height - 1]] $ \p@(x, y) -> do
      let (dx, dy) = pixelMapFn p

      pixel <- getPixel surface x y
      putPixel flipped dx dy pixel

  return flipped

  where pixelFormat = SDL.surfaceGetPixelFormat surface
