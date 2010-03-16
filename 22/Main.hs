{-# LANGUAGE Arrows, PatternGuards, ViewPatterns #-}

module Main (main)
       where

import Control.Arrow (returnA, (&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF,
                  tag, repeatedly, accumHoldBy)
import qualified Graphics.UI.SDL as SDL

import qualified Timer
import SDLUtils (loadImage, setColorKey)
import YampaUtils (reactimate)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

framesPerSecond = 20
ticksPerFrame   = 1 / framesPerSecond

data Dot = Dot Int Int

dotD            = 20
dotR            = dotD `div` 2

-- starting position of the dot controlled by user
dotX = 320
dotY = 240

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data GContext =
  GContext { c_screen :: SDL.Surface
           , c_bg     :: SDL.Surface
           , c_dot    :: SDL.Surface }

initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  bg     <- loadImage "bg.png"
  dot    <- (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage "dot.bmp"

  SDL.setCaption "Move the Dot" ""

  return $ GContext screen bg dot

quitSDL :: IO ()
quitSDL = SDL.quit

renderScreen :: GContext -> Dot -> IO ()
renderScreen (GContext screen bg dot) (Dot x y) = do
  fillBg
  drawDot x' y'
  SDL.flip screen

  where fillBg = do
          SDL.blitSurface bg (Just bgLeftR) screen Nothing
          SDL.blitSurface bg (Just bgRightR) screen (Just screenRightR)
          where cx' = cx `mod` screenWidth
                cw' = screenWidth - cx'

                bgLeftR      = SDL.Rect cx' cy cw' screenHeight
                bgRightR     = SDL.Rect 0   cy cx' screenHeight
                screenRightR = SDL.Rect cw' cy cx' screenHeight

        drawDot x y =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dotR) (y - dotR) 0 0)

        cx = x - screenWidth `div` 2
        cy = y - screenHeight `div` 2

        x' = x - cx
        y' = y - cy

main :: IO ()
main = do
  gcontext <- initSDL

  timer <- Timer.mkTimer
  let init  = Timer.start timer >> return False

  let sense = do
        event <- SDL.pollEvent

        let input =
              case event of
                SDL.Quit           -> Just $ True
                _                  -> Nothing

        when (isNothing input) $
          threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.restart timer

        if isNothing input
          then return (time, Just False)
          else return (time, input)

  let actuate Nothing    _       = return True
      actuate _          NoEvent = return False
      actuate (Just dot) _       = do
        renderScreen gcontext dot
        return False

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (dot beat &&& beat)

  quitSDL

dot :: SF () (Event ()) -> SF Bool (Maybe Dot)
dot beat =
  proc closed -> do
    beat' <- beat -< ()

    let vx = 2
    x <- accumHoldBy (+) dotX -< beat' `tag` vx

    returnA -< if closed then Nothing else Just (Dot x dotY)
