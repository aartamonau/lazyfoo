{-# LANGUAGE Arrows #-}

module Main (main)
       where

import Debug.Trace

import Data.Word (Word8)

import Control.Arrow ((&&&), returnA)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF,
                  filterE, isEvent, dAccumHoldBy, hold, tag, repeatedly)

import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode (..))

import qualified Timer
import SDLUtils (loadImage)
import YampaUtils (reactimate, flipE)

data GContext =
  GContext { screen :: SDL.Surface
           , back   :: SDL.Surface
           , front  :: SDL.Surface }

data Input =
  Input { keyDown :: Event SDL.SDLKey
        , keyUp   :: Event SDL.SDLKey
        , close   :: Event () }

type Alpha = Int
alphaTransparent, alphaOpaque :: Alpha
alphaTransparent = 0
alphaOpaque      = 255

screenWidth, screenHeight, screenBPP :: Int
screenWidth  = 640
screenHeight = 480
screenBPP    = 32

framesPerSecond :: Int
framesPerSecond = 20

ticksPerFrame :: Double
ticksPerFrame   = 1 / (fromIntegral framesPerSecond)

initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  back   <- loadImage "fadein.png"
  front  <- loadImage "fadeout.png"

  SDL.setCaption "Alpha Test" ""

  return $ GContext screen back front

quitSDL :: IO ()
quitSDL = do
  SDL.quit
  exitWith ExitSuccess

renderScreen :: GContext -> Alpha -> IO ()
renderScreen (GContext screen back front) alpha = do
  SDL.setAlpha front [SDL.SrcAlpha] (fromIntegral alpha)

  SDL.blitSurface back Nothing screen Nothing
  SDL.blitSurface front Nothing screen Nothing

  SDL.flip screen

main :: IO ()
main = do
  gcontext <- initSDL

  timer <- Timer.mkTimer
  let init  = Timer.start timer >> return emptyInput

  let sense = do
        event <- SDL.pollEvent

        let input =
              case event of
                SDL.Quit           -> Just $ Input NoEvent NoEvent (Event ())
                SDL.KeyDown keysym ->
                  Just $ Input (Event $ SDL.symKey keysym) NoEvent NoEvent
                SDL.KeyUp keysym ->
                  Just $ Input NoEvent (Event $ SDL.symKey keysym) NoEvent
                _                  -> Nothing

        when (isNothing input) $
          threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.restart timer

        if isNothing input
          then return (time, Just emptyInput)
          else return (time, input)

  let actuate Nothing      _       = return True
      actuate _            NoEvent = return False
      actuate (Just alpha) _       = do
        renderScreen gcontext alpha
        return False

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (alpha beat &&& beat)

  quitSDL

  where emptyInput :: Input
        emptyInput = Input NoEvent NoEvent NoEvent

alpha :: SF () (Event ()) -> SF Input (Maybe Alpha)
alpha beat =
  proc (Input keyDown keyUp close) -> do
    let downD = filterE (== SDL.SDLK_DOWN) keyDown
    let downU = filterE (== SDL.SDLK_DOWN) keyUp
    let upD   = filterE (== SDL.SDLK_UP)   keyDown
    let upU   = filterE (== SDL.SDLK_UP)   keyUp

    down <- hold False -< flipE downD downU
    up   <- hold False -< flipE upD upU

    beat' <- beat -< ()

    rec
      alpha <- dAccumHoldBy (+) alphaOpaque -< beat'
                                               `tag`
                                               (handleKeys down up alpha)

    returnA -< if isEvent close then Nothing else Just alpha

    where handleKeys :: Bool -> Bool -> Alpha -> Alpha
          handleKeys down up alpha = vd + vu
            where vd = if down && alpha > alphaTransparent then -5 else 0
                  vu = if up && alpha < alphaOpaque then 5 else 0

          trace' a = traceShow a a
          traceMsg msg a = trace (msg ++ " :" ++ show a) a
