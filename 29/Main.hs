{-# LANGUAGE Arrows #-}

module Main ( main )
       where


------------------------------------------------------------------------------
import Control.Arrow ( (&&&), returnA )
import Control.Concurrent ( threadDelay )
import Control.Monad ( forM, forM_, when )

------------------------------------------------------------------------------
import FRP.Yampa (SF, Event(..),
                  repeatedly, hold, filterE, tag, rMerge, dAccumHoldBy)

import qualified Graphics.UI.SDL as SDL


------------------------------------------------------------------------------
import qualified Timer
import SDLUtils ( loadImage, setColorKey, rectBoundaries, doRectsCollide )
import YampaUtils ( reactimate )


------------------------------------------------------------------------------
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

screenBpp :: Int
screenBpp = 32


------------------------------------------------------------------------------
dotR :: Int
dotR = 10

dotD :: Int
dotD = 2 * dotR

dotWidth :: Int
dotWidth = 2 * dotR

dotHeight :: Int
dotHeight = 2 * dotR

dotX :: Int
dotX = dotR

dotY :: Int
dotY = dotR

------------------------------------------------------------------------------
levelWidth :: Int
levelWidth = 1280

levelHeight :: Int
levelHeight = 960


------------------------------------------------------------------------------
data Tile = Red
          | Green
          | Blue
          | Center
          | Top
          | TopRight
          | Right
          | BottomRight
          | Bottom
          | BottomLeft
          | Left
          | TopLeft
          deriving (Enum, Show, Eq)


tileWidth :: Int
tileWidth = 80

tileHeight :: Int
tileHeight = 80


------------------------------------------------------------------------------
type Map = [[(SDL.Rect, Tile)]]

readMap :: FilePath -> IO Map
readMap path = do
  content <- fmap ((map words) . lines) $ readFile path
  let tiles = map (map (toEnum . read)) content
  return $ zipWith zip rects tiles

  where rects = [ [ SDL.Rect (j * tileWidth) (i * tileHeight)
                             tileWidth tileHeight
                        | j <- [0 ..] ]
                        | i <- [0 ..] ]



------------------------------------------------------------------------------
framesPerSecond :: Int
framesPerSecond = 30

ticksPerFrame :: Double
ticksPerFrame   = 1 / fromIntegral framesPerSecond


------------------------------------------------------------------------------
data Dot = Dot Int Int
         deriving Show


------------------------------------------------------------------------------
data GContext =
  GContext { c_screen ::  SDL.Surface
           , c_dot    ::  SDL.Surface
           , c_clips  :: [SDL.Surface]
           }


initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBpp [SDL.SWSurface]

  dot   <- loadImage' "dot.png"
  tiles <- loadImage' "tiles.png"

  SDL.setCaption "Tiles" ""

  clips <- forM [Red ..] $ \tile -> do
    surface <- SDL.createRGBSurface [SDL.SWSurface]
                                    tileWidth tileHeight screenBpp
                                    0 0 0 0
    let rect = SDL.Rect (fromEnum tile * tileWidth) 0 tileWidth tileHeight
    SDL.blitSurface tiles (Just rect) surface Nothing

    return surface

  return $ GContext screen dot clips

  where loadImage' image =
          (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage image


renderScreen :: GContext -> Map -> SDL.Rect -> Dot -> IO ()
renderScreen context map camera dot = do
  renderBg context map camera
  renderDot context camera dot
  SDL.flip $ c_screen context

renderDot :: GContext -> SDL.Rect -> Dot -> IO ()
renderDot (GContext screen dotSurface _) (SDL.Rect rx ry _ _) (Dot x y) = do
  SDL.blitSurface dotSurface Nothing screen (Just dstRect)
  return ()

  where dstRect = SDL.Rect (x - rx - dotR) (y - ry - dotR) dotD dotD

renderBg :: GContext -> Map -> SDL.Rect -> IO ()
renderBg (GContext screen _ clips) map camera =
  forM_ (concat map) $ \(tileRect, tileType) -> do
    when (visible tileRect) $ do
      SDL.blitSurface (clips !! fromEnum tileType) Nothing
                      screen (Just $ project tileRect)
      return ()

  where visible = doRectsCollide camera

        project (SDL.Rect x y w h) =
          SDL.Rect (x - SDL.rectX camera) (y - SDL.rectY camera) w h


------------------------------------------------------------------------------
main :: IO ()
main = do
  gcontext <- initSDL
  timer    <- Timer.mkTimer
  levelMap <- readMap "lazy.map"

  let init  = Timer.start timer >> return SDL.NoEvent
  let sense = do
        event <- SDL.pollEvent
        when (event == SDL.NoEvent) $ threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.restart timer

        return (time, Just event)
  let actuate  Nothing             _       = return True
      actuate  _                   NoEvent = return False
      actuate (Just dot@(Dot x y)) _       = do
        renderScreen gcontext levelMap camera dot
        return False

        where camera = SDL.Rect cx cy screenWidth screenHeight

              cx | left < 0           = 0
                 | right > levelWidth = levelWidth - screenWidth
                 | otherwise          = left
                where left  = x - screenWidth `div` 2
                      right = left + screenWidth

              cy | top < 0              = 0
                 | bottom > levelHeight = levelHeight - screenHeight
                 | otherwise            = top
                where top    = y - screenHeight `div` 2
                      bottom = top + screenHeight

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (dot levelMap beat &&& beat)

  SDL.quit


------------------------------------------------------------------------------
keyDown :: SDL.Event -> Event SDL.SDLKey
keyDown (SDL.KeyDown keysym) = Event $ SDL.symKey keysym
keyDown  _                   = NoEvent


keyUp :: SDL.Event -> Event SDL.SDLKey
keyUp (SDL.KeyUp keysym) = Event $ SDL.symKey keysym
keyUp  _                 = NoEvent


quit :: SDL.Event -> Bool
quit SDL.Quit = True
quit _        = False


dot :: Map -> SF () (Event ()) -> SF SDL.Event (Maybe Dot)
dot levelMap beat =
  proc event -> do
    let up   = keyUp   event
    let down = keyDown event

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

    returnA -< if quit event then Nothing else Just $ Dot x y

    where v :: SF (Event a, Event b) Int
          v = proc (activate, deactivate) ->
                hold 0 -< activate `tag` dv
                          `rMerge`
                          deactivate `tag` 0

          dv = dotR

          dotToRect :: (Int, Int) -> SDL.Rect
          dotToRect (x, y) = SDL.Rect (x - dotR) (y - dotR) dotD dotD

          respectBoundaries :: SF ((Int, Int), (Int, Int)) (Int, Int)
          respectBoundaries =
            proc ((x, y), (vx, vy)) -> do
              let rvx = if hasAnyCollisions (x + vx, y) then 0 else vx
              let rvy = if hasAnyCollisions (x + rvx, y + vy) then 0 else vy

              returnA -< (rvx, rvy)

            where hasAnyCollisions dot =
                    breaksBoundaries dot || collides dot

          collides :: (Int, Int) -> Bool
          collides dot = or $ map (doRectsCollide (dotToRect dot)) holes

          holes = map fst $ filter pred (concat levelMap)
            where pred (_, tileType)
                    | tileType `elem` [Red, Green, Blue] = False
                    | otherwise = True

          breaksBoundaries :: (Int, Int) -> Bool
          breaksBoundaries dot =
            or [left < 0, top < 0, right > levelWidth, bottom > levelHeight]
            where (left, right, top, bottom) = rectBoundaries $ dotToRect dot
