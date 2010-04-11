{-# LANGUAGE Arrows, PatternGuards #-}

module Main (main)
       where

import Control.Applicative ((<*>), pure)
import Control.Arrow ((&&&), (>>>), returnA, arr)
import Control.Concurrent (threadDelay)
import Control.Monad (when, mapM_, forM_)

import Data.Maybe (isNothing, fromJust)
import Data.Word (Word8)

import FRP.Yampa (Event(..), SF,
                  noise, loopPre,
                  filterE, tag, repeatedly,
                  rMerge, hold, dHold,
                  dAccumHoldBy, isEvent)
import qualified Graphics.UI.SDL as SDL

import System.Random (StdGen, Random, getStdGen, split)

import ArrowUtils (sequenceA)
import qualified Timer
import SDLUtils (loadImage, setColorKey)
import YampaUtils (reactimate)

screenWidth, screenHeight, screenBPP :: Int
screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

totalParticles :: Int
totalParticles = 20

framesPerSecond :: Int
framesPerSecond = 20

ticksPerFrame :: Double
ticksPerFrame   = 1 / (fromIntegral framesPerSecond)

data Dot =
  Dot { dot_x  :: Int
      , dot_y  :: Int }

data ParticleType = Red | Green | Blue

data Particle =
  Particle { particle_x     :: Int
           , particle_y     :: Int
           , particle_frame :: Int
           , particle_type  :: ParticleType }

dotD, dotR, dotX, dotY :: Int
dotD = 20
dotR = dotD `div` 2
dotX = 10
dotY = 10

data Input =
  Input { keyDown :: Event SDL.SDLKey
        , keyUp   :: Event SDL.SDLKey
        , closed  :: Bool
        }

data GContext =
  GContext { c_screen  :: SDL.Surface
           , c_dot     :: SDL.Surface
           , c_red     :: SDL.Surface
           , c_green   :: SDL.Surface
           , c_blue    :: SDL.Surface
           , c_shimmer :: SDL.Surface
           , c_bg      :: SDL.Pixel }

initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen  <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  dot     <- loadImage' "dot.bmp"

  red     <- loadImage' "red.bmp"
  green   <- loadImage' "green.bmp"
  blue    <- loadImage' "blue.bmp"
  shimmer <- loadImage' "shimmer.bmp"

  forM_ [red, green, blue, shimmer] $ \surface -> do
    SDL.setAlpha surface [SDL.SrcAlpha, SDL.RLEAccel] 192

  bg      <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff

  SDL.setCaption "Particle test" ""

  return $ GContext screen dot red green blue shimmer bg
  where loadImage' image =
          (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage image

quitSDL :: IO ()
quitSDL = SDL.quit

renderScreen :: GContext -> Dot -> [Particle] -> IO ()
renderScreen (GContext screen dot red green blue shimmer bg) (Dot x y) ps = do
  fillBg
  drawDot

  mapM drawParticle ps

  SDL.flip screen

  where fillBg      = SDL.fillRect screen Nothing bg

        drawCircle surface r x y = do
          SDL.blitSurface surface Nothing
                          screen (Just $ SDL.Rect (x - r) (y - r) 0 0)
          return ()

        drawDot = drawCircle dot dotR x y

        drawParticle (Particle x y frame ptype) = do
          drawCircle typeSurface r x y
          when (frame `mod` 2 == 0) $ drawCircle shimmer r x y
          where r = 3
                typeSurface | Red   <- ptype     = red
                            | Green <- ptype     = green
                            | Blue  <- ptype     = blue


main :: IO ()
main = do
  gcontext <- initSDL

  timer <- Timer.mkTimer
  gen   <- getStdGen
  let init  = Timer.start timer >> return (Input NoEvent NoEvent False)

  let sense = do
        event <- SDL.pollEvent

        let input =
              case event of
                SDL.Quit           -> Just $ Input NoEvent NoEvent True
                SDL.KeyDown keysym ->
                  Just $ Input (Event $ SDL.symKey keysym) NoEvent False
                SDL.KeyUp keysym ->
                  Just $ Input NoEvent (Event $ SDL.symKey keysym) False
                _                  -> Nothing

        when (isNothing input) $
          threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.restart timer

        if isNothing input
          then return (time, Just defaultInput)
          else return (time, input)

  let actuate (Nothing, _)                _       = return True
      actuate _                           NoEvent = return False
      actuate (Just dot, Just particles)  _       = do
        renderScreen gcontext dot particles
        return False

  let beat = repeatedly ticksPerFrame ()

  let wrap arr =
        proc x -> do
          result <- arr -< fromJust x
          returnA -< if isNothing x then Nothing else Just result
  let sf   = dot beat >>> (arr id &&& (wrap $ particles gen beat)) &&& beat

  reactimate init sense (uncurry actuate) sf

  quitSDL

  where defaultInput = Input NoEvent NoEvent False

dot :: SF () (Event ()) -> SF Input (Maybe Dot)
dot beat =
  proc (Input down up closed) -> do
    let leftD  = filterE (== SDL.SDLK_LEFT)  down
    let leftU  = filterE (== SDL.SDLK_LEFT)  up
    let rightD = filterE (== SDL.SDLK_RIGHT) down
    let rightU = filterE (== SDL.SDLK_RIGHT) up

    let upD    = filterE (== SDL.SDLK_UP)    down
    let upU    = filterE (== SDL.SDLK_UP)    up
    let downD  = filterE (== SDL.SDLK_DOWN)  down
    let downU  = filterE (== SDL.SDLK_DOWN)  up

    vl <- v -< (leftD, leftU)
    vr <- v -< (rightD, rightU)
    vu <- v -< (upD, upU)
    vd <- v -< (downD, downU)

    let vx = vr - vl
    let vy = vd - vu

    beat' <- beat -< ()

    rec
      (rvx, rvy) <- respectBoundaries -< ((x, y), (vx, vy))

      x <- dAccumHoldBy (+) dotX -< beat' `tag` rvx
      y <- dAccumHoldBy (+) dotY -< beat' `tag` rvy

    returnA -< if closed then Nothing else Just $ Dot x y

  where v :: SF (Event a, Event b) Int
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` dv
                        `rMerge`
                        deactivate `tag` 0

        dv = dotR

        respectBoundaries :: SF ((Int, Int), (Int, Int)) (Int, Int)
        respectBoundaries =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if breaksBoundaries (x + vx, y) then 0 else vx
            let rvy = if breaksBoundaries (x, y + vy) then 0 else vy

            returnA -< (rvx, rvy)

        breaksBoundaries :: (Int, Int) -> Bool
        breaksBoundaries (x, y) =
          or [left < 0, top < 0, bottom > screenHeight, right > screenWidth]
          where left   = x - dotR
                right  = x + dotR
                top    = y - dotR
                bottom = y + dotR

particles :: StdGen -> SF () (Event ()) -> SF Dot [Particle]
particles gen beat = sequenceA (map particle gens <*> pure beat)
  where gens = take totalParticles $ map snd $
                 tail $ iterate (split . fst) (gen, undefined)

particle :: StdGen -> SF () (Event ()) -> SF Dot Particle
particle gen beat =
  proc dot -> do
    beat' <- beat -< ()

    loopPre fakeParticle particle' -< (beat', dot)

  where (genType, gen')   = split gen
        (genFrame, gen'') = split gen'
        (genX, genY)      = split gen''

        particleLifeLimit = 10
        fakeParticle = Particle 0 0 (particleLifeLimit + 1) Red

        particle' :: SF ((Event (), Dot), Particle) (Particle, Particle)
        particle' =
          proc ((beat, Dot x y), p@(Particle _ _ pframe _)) -> do
            rx     <- noise genX -< ()
            ry     <- noise genY -< ()
            rtype  <- noise genType -< ()
            rframe <- noise genFrame -< ()

            let px'     = x - 5 + rx `mod` 25
            let py'     = y - 5 + ry `mod` 25
            let ptype'  = intToType rtype
            let pframe' = rframe `mod` (particleLifeLimit `div` 2)

            let result = if pframe > particleLifeLimit
                           then Particle px' py' pframe' ptype'
                           else p { particle_frame = pframe + 1 }

            returnA -< if isEvent beat then (result, result) else (p, p)

        intToType :: Int -> ParticleType
        intToType 0 = Red
        intToType 1 = Green
        intToType 2 = Blue
        intToType n = intToType $ n `mod` 3
