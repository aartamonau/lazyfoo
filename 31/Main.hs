module Main
       (
         main
       ) where

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL

import SDLUtils (loadImage, flipSurface, FlipFlag (..))

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

screenBpp :: Int
screenBpp = 32

initSDL :: IO SDL.Surface
initSDL = do
  SDL.init [SDL.InitEverything]
  screen <- SDL.setVideoMode screenWidth screenHeight screenBpp [SDL.SWSurface]

  let format = SDL.surfaceGetPixelFormat screen
  white <- SDL.mapRGB format 0xff 0xff 0xff

  SDL.fillRect screen Nothing white

  return screen

main :: IO ()
main = do
  screen <- initSDL

  topLeft     <- loadImage "corner.png"
  topRight    <- flipSurface topLeft [FlipHorizontal]
  bottomLeft  <- flipSurface topLeft [FlipVertical]
  bottomRight <- flipSurface topLeft [FlipHorizontal, FlipVertical]

  SDL.blitSurface topLeft     Nothing screen (Just $ SDL.Rect 0   0   0 0)
  SDL.blitSurface topRight    Nothing screen (Just $ SDL.Rect 320 0   0 0)
  SDL.blitSurface bottomLeft  Nothing screen (Just $ SDL.Rect 0   240 0 0)
  SDL.blitSurface bottomRight Nothing screen (Just $ SDL.Rect 320 240 0 0)

  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.Quit -> return ()
      _        -> loop

  SDL.quit
