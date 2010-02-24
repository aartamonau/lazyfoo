module Main (main)
       where

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL.Image

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "Summer almost gone!" ""

  bg     <- loadImage "background.png"

  person <- loadImage "person.png"
  setColorKey person (0x0, 0xff, 0xff)

  applySurface   0   0     bg screen
  applySurface 240 190 person screen

  SDL.flip screen

  waitForClick

  SDL.quit

waitForClick = do
  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.MouseButtonUp _ _ SDL.ButtonLeft -> return ()
      _                                    -> loop

loadImage path = do
  image   <- SDL.Image.load path
  dfImage <- SDL.displayFormat image

  SDL.freeSurface image

  return dfImage

setColorKey surface (r, g, b) = do
  let format =  SDL.surfaceGetPixelFormat surface
  colorkey   <- SDL.mapRGB format r g b

  SDL.setColorKey surface [SDL.SrcColorKey] colorkey

applySurface x y src dst = do
  SDL.Rect _ _ w h <- SDL.getClipRect src
  SDL.blitSurface src Nothing dst (Just $ SDL.Rect x y 0 0)
