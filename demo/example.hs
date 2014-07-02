import Control.Monad
import Data.IORef
import Data.Monoid
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators             as Draw
import qualified Graphics.UI.GLFW                        as GLFW

import System.Environment(getArgs)

resX, resY :: Int
resX = 640
resY = 480

initScreen :: IO GLFW.Window
initScreen = do
  True <- GLFW.init
  -- Do we want to give these window hints?
-- [GLFW.DisplayRGBBits 8 8 8,
--  GLFW.DisplayDepthBits 8]
  Just win <-
    GLFW.createWindow
    (fromIntegral resX)
    (fromIntegral resY)
    "Graphics-drawingcombinators demo"
    Nothing Nothing
  GLFW.makeContextCurrent $ Just win
  return win

unitText :: Draw.Font -> String -> Draw.Image Any
unitText font str = (Draw.translate (-1,0) %% Draw.scale (2/w) (2/w) %% Draw.text font str)
                        `mappend`
                    Draw.tint (Draw.Color 1 0 0 1) (Draw.line (-1,0) (1,0))
    where
    w = Draw.textWidth font str

quadrants :: (Monoid a) => Draw.Image a -> Draw.Image a
quadrants img = mconcat [
    (Draw.translate (-0.5,0.5) %%),
    (Draw.translate (0.5,0.5)   `Draw.compose` Draw.rotate (-pi/2) %%),
    (Draw.translate (0.5,-0.5)  `Draw.compose` Draw.rotate pi %%),
    (Draw.translate (-0.5,-0.5) `Draw.compose` Draw.rotate (pi/2) %%)] (Draw.scale 0.5 0.5 %% img)

circleText :: Draw.Font -> String -> Draw.Image Any
circleText font str = unitText font str `mappend` Draw.tint (Draw.Color 0 0 1 0.5) Draw.circle

main :: IO ()
main = do
    win <- initScreen
    args <- getArgs
    font <- case args of
        [fontName] -> do
            font <- Draw.openFont fontName
            return font
        _ -> error "Usage: drawingcombinators-example some_font.ttf"


    doneRef <- newIORef False
    GLFW.setWindowCloseCallback win $ Just $ const $ writeIORef doneRef True
    waitClose win font doneRef 0
    GLFW.terminate
    return ()
    where
      waitClose win font doneRef rotation = do
        isDone <- readIORef doneRef
        unless isDone $ do
          Draw.clearRender $ Draw.rotate rotation %% quadrants (circleText font "Hello, World!")
          GLFW.swapBuffers win
          GLFW.pollEvents
          waitClose win font doneRef $ rotation - 0.01
