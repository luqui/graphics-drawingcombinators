import Control.Applicative
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


fromAny :: Alternative f => a -> Any -> f a
fromAny _ (Any False) = empty
fromAny msg (Any True) = pure msg

circleText :: Draw.Font -> String -> Draw.Image [String]
circleText font str =
  mconcat
  [ fromAny str <$> unitText font str
  , fromAny "circle" <$> Draw.tint (Draw.Color 0 0 1 0.5) Draw.circle
  ]

toGLCoors :: GLFW.Window -> (Double, Double) -> IO Draw.R2
toGLCoors win (x, y) = do
    (w, h) <- GLFW.getFramebufferSize win
    return (coor x w, - coor y h)
    where
        coor a b = realToFrac (2 * a / fromIntegral b - 1)

main :: IO ()
main = do
    win <- initScreen
    args <- getArgs
    (font, pic) <- case args of
        [fontName, picName] -> do
            font <- Draw.openFont fontName
            pic <- Draw.openSprite picName
            return (font, pic)
        _ -> error "Usage: drawingcombinators-example some_font.ttf some_img.png"


    doneRef <- newIORef False
    GLFW.setWindowCloseCallback win $ Just $ const $ writeIORef doneRef True

    let mkImage rotation =
            Draw.rotate rotation %%
            quadrants
            ( mconcat
              [ Draw.scale 0.2 0.2 %%
                fromAny "sprite" <$> Draw.sprite pic
              , circleText font "Hej, World!"
              ] )
    imageRef <- newIORef $ mkImage 0
    GLFW.setMouseButtonCallback win $ Just $ const $ \_button press _mods ->
        when (press == GLFW.MouseButtonState'Pressed) $ do
            image <- readIORef imageRef
            pos <- GLFW.getCursorPos win
            glPos <- toGLCoors win pos
            let strs = Draw.sample image glPos
            print strs
    let waitClose rotation = do
            isDone <- readIORef doneRef
            unless isDone $ do
                let image = mkImage rotation
                writeIORef imageRef image
                Draw.clearRender image
                GLFW.swapBuffers win
                GLFW.pollEvents
                waitClose $ rotation - 0.01
    waitClose 0
    GLFW.terminate
    return ()
