{-# LANGUAGE ImplicitParams #-}

module Main
       (
         main
       ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.MVar ( MVar,
                                 newMVar, newEmptyMVar, takeMVar, putMVar )
import Control.Exception ( bracket_ )
import Control.Monad ( forM, forM_, when )
import Data.Function ( fix )

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import SDLUtils ( loadImage )

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

textColor = SDL.Color 0 0 0

withVideoLock :: (?lock :: MVar ()) => IO () -> IO ()
withVideoLock =
  bracket_ (takeMVar ?lock) (putMVar ?lock ())

showSurface :: (?lock :: MVar (),
                ?screen :: SDL.Surface) =>
               Int -> Int -> SDL.Surface -> IO ()
showSurface x y source =
  withVideoLock $ do
    SDL.blitSurface source Nothing ?screen (Just $ SDL.Rect x y 0 0)
    SDL.flip ?screen

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

thread :: (?lock :: MVar (),
           ?screen :: SDL.Surface) =>
          Int -> [SDL.Surface] -> IO ()
thread offset surfaces = do
  forM_ (enumerate surfaces) $ \(i, surf) -> do
    let width = SDL.surfaceGetWidth surf
    let x     = offset + (((screenWidth `div` 2) - width) `div` 2)
    let y     = 10 + i * 100

    threadDelay 200000
    showSurface x y surf

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
  TTF.init

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  bg     <- loadImage "background.png"
  font   <- TTF.openFont "lazy.ttf" 72

  text <- forM ["One", "Two", "Three", "Four", "Five"] $ \t ->
            TTF.renderTextSolid font t textColor

  lock <- newMVar ()

  -- ugly
  let ?lock = lock
  let ?screen = screen

  showSurface 0 0 bg

  t1 <- spawn $ thread 0 text
  t2 <- spawn $ thread (screenWidth `div` 2) text

  wait t1
  wait t2

  fix $ \loop -> do
    event <- SDL.pollEvent
    when (event /= SDL.Quit) loop

  return ()
