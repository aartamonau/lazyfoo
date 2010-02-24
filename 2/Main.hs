module Main (main)
       where

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "Hello" ""

  message <- loadImageWithColorkey "hello.bmp" (0xff, 0x0, 0x0)
  bg      <- loadImage "background.bmp"

  applySurface   0   0 bg screen
  applySurface 320   0 bg screen
  applySurface   0 240 bg screen
  applySurface 320 240 bg screen

  applySurface 180 140 message screen

  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.MouseButtonUp _ _ SDL.ButtonLeft -> return ()
      _                                    -> loop

  SDL.quit

loadImage path = do
  image   <- SDL.loadBMP path
  dfImage <- SDL.displayFormat image

  SDL.freeSurface image

  return dfImage

applySurface x y src dst = do
  SDL.blitSurface src Nothing dst (Just $ SDL.Rect x y 0 0)

setColorKey surface (r, g, b) = do
  let format =  SDL.surfaceGetPixelFormat surface
  colorkey   <- SDL.mapRGB format r g b

  SDL.setColorKey surface [SDL.SrcColorKey] colorkey

loadImageWithColorkey path colorkey = do
  image <- loadImage path
  setColorKey image colorkey

  return image
