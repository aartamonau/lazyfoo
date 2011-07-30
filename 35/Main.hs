module Main
       (
         main
       ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.MVar ( MVar,
                                 newMVar ,newEmptyMVar,
                                 readMVar, takeMVar, putMVar )
import Control.Monad ( forM, forM_, when )
import Data.Function ( fix )
import System.Random ( randomRIO )

import qualified Graphics.UI.SDL as SDL

import SDLUtils ( loadImage )

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

data Context =
  Context { screen     :: SDL.Surface
          , images     :: [SDL.Surface]
          , canProduce :: MVar ()
          , buffer     :: MVar (SDL.Surface)
          }

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

producer :: Context -> IO ()
producer (Context screen images canProduce buffer) = do
  forM_ (enumerate images) $ \(i, image) -> do
    delay <- randomRIO (0, 1000)
    threadDelay (delay * 1000)

    _ <- takeMVar canProduce

    ix <- randomRIO (0, n - 1)
    let image = images !! ix
    let y     = 10 + i * 90

    SDL.blitSurface image Nothing screen (Just $ SDL.Rect 10 y 0 0)
    SDL.flip screen

    putMVar buffer image

  where n = length images

consumer :: Context -> IO ()
consumer (Context screen images canProduce buffer) = do
  forM_ (enumerate images) $ \(i, _) -> do
    delay <- randomRIO (0, 1000)
    threadDelay (delay * 1000)

    let y  = 10 + i * 90
    image <- takeMVar buffer

    SDL.blitSurface image Nothing screen (Just $ SDL.Rect 330 y 0 0)
    SDL.flip screen

    putMVar canProduce ()

type Thread = MVar ()

spawn :: IO () -> IO Thread
spawn action = do
  thread <- newEmptyMVar
  _ <- forkIO (action >> putMVar thread ())
  return thread

wait :: Thread -> IO ()
wait = takeMVar

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  bg     <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff

  images <-
    forM (map ((++".png") . show) [1..5]) $ \path -> do
      loadImage path

  SDL.fillRect screen Nothing bg
  SDL.flip screen

  SDL.setCaption "Producer/Consumer" ""

  buffer     <- newEmptyMVar
  canProduce <- newMVar ()

  let ctx = Context screen images canProduce buffer

  pt <- spawn (producer ctx)
  ct <- spawn (consumer ctx)

  wait pt
  wait ct

  fix $ \loop -> do
    event <- SDL.pollEvent
    when (event /= SDL.Quit) loop

  return ()
