module Main ( main )
       where


import Data.Function ( fix )


import qualified Graphics.UI.SDL as SDL


import Font ( createFromFile, showText )

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

  font <- createFromFile "lazyfont.png"
  -- showText font "Bitmap Font:\ntest test testA" screen 0 0
  showText font "Bitmap Font:\nABDCEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789"
           screen 100 100
  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.Quit -> return ()
      _        -> loop

  SDL.quit
