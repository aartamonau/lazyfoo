module Main
       (
         main
       ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.MVar ( MVar,
                                 newEmptyMVar, takeMVar, tryTakeMVar, putMVar )
import Control.Monad ( when, unless )
import Data.Function ( fix )

import qualified Graphics.UI.SDL as SDL

import SDLUtils ( loadImage )

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

thread :: MVar () -> IO ()
thread quit = loop
  where loop :: IO ()
        loop = do
          quit' <- tryTakeMVar quit

          case quit' of
            Just () ->
              return ()
            Nothing -> do
              SDL.setCaption "Thread is running" ""
              threadDelay 250000

              SDL.setCaption "Thread is running." ""
              threadDelay 250000

              SDL.setCaption "Thread is running.." ""
              threadDelay 250000

              SDL.setCaption "Thread is running..." ""
              threadDelay 250000

              loop

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

  image <- loadImage "image.png"
  SDL.blitSurface image Nothing screen Nothing

  SDL.setCaption "Thread test" ""

  SDL.flip screen

  quit <- newEmptyMVar

  t <- spawn (thread quit)

  fix $ \loop -> do
    event <- SDL.pollEvent
    when (event /= SDL.Quit) loop

  putMVar quit ()
  wait t

  return ()
