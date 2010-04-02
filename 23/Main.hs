module Main ( main )
       where

import Control.Monad (forM_, when, unless)

import Data.Char (isAlphaNum)
import Data.Function (fix)
import Data.IORef (newIORef, modifyIORef, readIORef, writeIORef)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import System (exitWith, ExitCode(..))

import SDLUtils (loadImage)
import TTFUtils (applyText)
import Utils (takeWhileM)

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

textColor = SDL.Color 0xff 0xff 0xff

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.enableUnicode True
  TTF.init

  screen  <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  bg      <- loadImage "background.png"
  font    <- TTF.openFont "lazy.ttf" 42

  SDL.setCaption "High Score" ""

  textRef        <- newIORef ""
  nameEnteredRef <- newIORef False

  fix $ \loop -> do
    events <- takeWhileM (/= SDL.NoEvent) $ repeat SDL.pollEvent

    forM_ events $ \event -> do
      case event of
        SDL.Quit           -> quit
        SDL.KeyDown keysym -> do
          nameEntered <- readIORef nameEnteredRef

          unless nameEntered $ do
            let sym = SDL.symUnicode keysym
            text <- readIORef textRef

            if isAlphaNum sym
              then when (length text < 16) $ modifyIORef textRef (sym:)
              else do
                let key = SDL.symKey keysym

                case key of
                  SDL.SDLK_BACKSPACE ->
                    unless (null text) $
                      modifyIORef textRef tail
                  SDL.SDLK_RETURN    -> writeIORef nameEnteredRef True
                  _                  -> return ()
        _                  -> return ()

    SDL.blitSurface bg Nothing screen Nothing

    nameEntered <- readIORef nameEnteredRef
    let message | nameEntered = "Rank: 1st"
                | otherwise   = "New High Score! Enter Name:"

    applyText message font textColor
              (screenHeight `div` 4) screen

    text <- readIORef textRef

    unless (null text) $
      applyText (reverse text) font textColor (screenHeight `div` 2)  screen

    SDL.flip screen

    loop

  where quit = do
          TTF.quit
          SDL.quit
          exitWith ExitSuccess