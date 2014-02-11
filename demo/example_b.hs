import Control.Monad
import Data.Monoid
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators             as Draw
import qualified Graphics.UI.GLFW                        as GLFW
import Data.IORef
import Data.Maybe

import System.Environment(getArgs)

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

initGL :: IO GLFW.Window
initGL = do
    isInited <- GLFW.init
    unless isInited $ error "Cannot init GLFW"
    
    maybeW <- GLFW.createWindow windowWidth windowHeight "DrawingCombinators demo" Nothing Nothing
    let window = fromMaybe (error "No window was created") maybeW
    GLFW.makeContextCurrent $ Just window
    
    return window

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
    font <- case args of
        [fontName] -> Draw.openFont fontName
        _          -> error "Usage: drawingcombinators-example some_font.ttf"
    
    window <- initGL
    
    done <- newIORef False
    GLFW.setWindowCloseCallback window (Just $ const $ writeIORef done True)
    mainLoop window font done 0.0
    
    where
        mainLoop window font done rotation = do
            Draw.clearRender $ Draw.rotate rotation %% quadrants (circleText font "Hello, world!")
            GLFW.swapBuffers window
            GLFW.pollEvents
            doneValue <- readIORef done
            when (not doneValue) $
                mainLoop window font done (rotation - 0.01)
