import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.UI.SDL as SDL
import Data.Monoid
import Data.List (isSuffixOf)

import System.Environment(getArgs)

resX, resY :: Int
resX = 640
resY = 480

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
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
    initScreen
    args <- getArgs
    font <- case args of
        [fontName] -> do
            font <- Draw.openFont fontName
            return font
        _ -> fail "Usage: drawingcombinators-example some_font.ttf"
        
        
    Draw.clearRender (quadrants (circleText font "Hello, World!"))
    SDL.glSwapBuffers
    waitClose
    SDL.quit
    return ()
    where

    waitClose = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             _ -> waitClose
