module Main (main)
       where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT (..), runReaderT, ask, asks)
import Control.Monad.State (StateT (..), evalStateT, get, gets, put)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode (..))

import SDLUtils (loadImage)

data WindowState =
  WindowState { ws_width  :: Int
              , ws_height :: Int }

data ProgramConfig =
  ProgramConfig { pc_bpp          :: Int
                , pc_screenWidth  :: Int
                , pc_screenHeight :: Int
                , pc_image        :: SDL.Surface }

data ProgramState =
  ProgramState { ps_screen      :: SDL.Surface
               , ps_windowState :: WindowState
               , ps_windowed    :: Bool
               , ps_width       :: Int
               , ps_height      :: Int }

type ProgramMonad = ReaderT ProgramConfig (StateT ProgramState IO)

runProgram :: ProgramMonad a -> IO a
runProgram p = do
  SDL.init [SDL.InitEverything]

  (sw, sh) <- getScreenDimensions
  state    <- getState
  config   <- getConfig sw sh

  evalStateT (runReaderT p config) state

  where getState :: IO ProgramState
        getState = do
          screen <- resizeWindowIO width height screenBPP
          SDL.setCaption "Window Event Test" ""

          return $ ProgramState screen ws True width height

          where width  = 640
                height = 480
                ws     = WindowState width height


        -- must be called before any call to SDL.setVideoMode to return correct
        -- value
        getScreenDimensions :: IO (Int, Int)
        getScreenDimensions = do
          videoInfo <- SDL.getVideoInfo

          -- patched SDL used here as SDL.videoInfoWidth and SD.videoInfoHeight
          -- must have type SDL.VideoInfo -> IO Int
          ((,)) <$> SDL.videoInfoWidth videoInfo
                <*> SDL.videoInfoHeight videoInfo

        getConfig :: Int -> Int -> IO ProgramConfig
        getConfig w h = ProgramConfig screenBPP w h <$> loadImage "window.png"

        screenBPP :: Int
        screenBPP = 32

io :: MonadIO m => IO a -> m a
io = liftIO

quitProgram :: ProgramMonad ()
quitProgram =
  liftIO $ do
    SDL.quit
    exitWith ExitSuccess

resizeWindowIO :: Int -> Int -> Int -> IO SDL.Surface
resizeWindowIO w h bpp = SDL.setVideoMode w h bpp [SDL.SWSurface, SDL.Resizable]

resizeWindow :: Int -> Int -> ProgramMonad ()
resizeWindow w h = do
  bpp <- asks pc_bpp

  screen <- io $ resizeWindowIO w h bpp
  put $ ProgramState screen (WindowState w h) True w h

toggleFullscreen :: ProgramMonad ()
toggleFullscreen = do
  windowed <- gets ps_windowed
  ProgramConfig bpp screenWidth screenHeight _ <- ask

  if windowed
    then do
      screen <- io $ SDL.setVideoMode screenWidth screenHeight bpp
                                      [SDL.SWSurface, SDL.Fullscreen]
      ws     <- gets ps_windowState
      put $ ProgramState screen ws False screenWidth screenHeight
    else do
      WindowState w h <- gets ps_windowState
      resizeWindow w h

renderWindow :: ProgramMonad ()
renderWindow = do
  ProgramState screen _ _ w h <- get
  let format = SDL.surfaceGetPixelFormat screen
  white <- io $ SDL.mapRGB format 0xff 0xff 0xff

  io $ SDL.fillRect screen Nothing white

  image <- asks pc_image
  (iw, ih) <- fmap (SDL.rectW &&& SDL.rectH) (io $ SDL.getClipRect image)
  let dstRect = SDL.Rect ((w - iw) `div` 2) ((h - ih) `div` 2) 0 0
  io $ SDL.blitSurface image Nothing screen (Just dstRect)

  io $ SDL.flip screen

loop :: ProgramMonad ()
loop = do
  renderWindow

  event <- io SDL.waitEventBlocking

  case event of
    SDL.Quit            -> quitProgram
    SDL.KeyDown keysym  ->
      case SDL.symKey keysym of
        SDL.SDLK_ESCAPE -> quitProgram
        SDL.SDLK_RETURN -> toggleFullscreen
        _               -> return ()
    SDL.VideoResize w h -> do
      resizeWindow w h
    SDL.VideoExpose     ->
      io . SDL.flip =<< gets ps_screen
    SDL.GotFocus _      ->
      io $ SDL.setCaption "Window Event Test" ""
    SDL.LostFocus focus ->
      mapM_ (setCaption . msg) focus

      where msg SDL.ApplicationFocus =
              "Window Event Test: Iconified"
            msg SDL.MouseFocus       =
              "Window Event Test: Mouse Focus Lost"
            msg SDL.InputFocus       =
              "Window Event Test: Keyboard focus lost"

            setCaption message = io $ SDL.setCaption message ""
    _                   -> return ()

  loop

main :: IO ()
main = runProgram loop