import Prelude hiding (reverse, sequence)
import Control.Monad hiding (sequence)
import Control.Concurrent
import Data.IORef
import Data.Monoid
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators.Animation (runAnimation, animate, scale, rotate, translate, reverse, parallel, sequence)
import qualified Graphics.UI.GLFW as GLFW

import System.Environment(getArgs)

resX, resY :: Int
resX = 640
resY = 480

initScreen :: IO ()
initScreen = do
  True <- GLFW.initialize
  True <- GLFW.openWindow GLFW.defaultDisplayOptions {
    GLFW.displayOptions_width = resX,
    GLFW.displayOptions_height = resY
    }

  return ()

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
    args <- getArgs
    (font, sprite) <- case args of
        [fontName, spriteName] -> do
            font <- Draw.openFont fontName
            sprite <- Draw.openSprite spriteName
            return (font, sprite)
        _ -> error "Usage: drawingcombinators-example some_font.ttf"


    initScreen
    doneRef <- newIORef False
    GLFW.setWindowCloseCallback $ do
      writeIORef doneRef True
      return True

    begin <- getPOSIXTime
    waitClose begin 0 doneRef $ quadrants (circleText sprite font "Hello, World!")
    GLFW.terminate
    return ()
    where

    animate' = sequence
                [ animate (scale (1, 1)
                       <> translate (1, 1)
                       <> rotate 3.14)       10
                , animate (translate (2, 2)) 5
                ]

    waitClose begin elapsed' doneRef image = do
      isDone <- readIORef doneRef
      unless isDone $ do
        elapsed <- fmap (begin `subtract`) getPOSIXTime
        Draw.clearRender (runAnimation elapsed animate' image)
        GLFW.swapBuffers
        GLFW.pollEvents
        waitClose begin elapsed doneRef image
