import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Data.Monoid

resX = 640
resY = 480

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    Draw.init
    return ()

textBox :: Draw.Color -> Draw.Font -> String -> Draw.Image (Maybe String)
textBox color font text = fmap (\(Any b) -> if b then Just text else Nothing) $
                            Draw.tint color (Draw.convexPoly [(1,1),(1,-1),(-1,-1),(-1,1)])
                                `mappend`
                            Draw.text font text

juxtapose :: (Monoid a) => Draw.Image a -> Draw.Image a -> Draw.Image a
juxtapose d1 d2 = (Draw.translate (-1,0) Draw.%% Draw.scale 0.5 1 Draw.%% d1)
                    `mappend`
                  (Draw.translate (1,0) Draw.%% Draw.scale 0.5 1 Draw.%% d2)

drawing :: Draw.Font -> Draw.Image (Maybe String)
drawing font = juxtapose (textBox (Draw.Color 1 0 0 1) font "A") 
                         (textBox (Draw.Color 0 0 1 1) font "B")

main :: IO ()
main = do
    initScreen
    font <- Draw.openFont "font.ttf" 72
    Draw.clearRender (drawing font)
    SDL.glSwapBuffers
    waitClicks font
    SDL.quit
    return ()

    where
    waitClicks font = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             SDL.MouseButtonDown x y _ -> do
                 let x' = 2*(fromIntegral x / fromIntegral resX) - 1
                 let y' = 1 - 2*(fromIntegral y / fromIntegral resY)
                 hit <- Draw.sample (x',y') (drawing font)
                 case hit of
                      Nothing   -> waitClicks font
                      Just str  -> putStrLn str >> waitClicks font
             _ -> waitClicks font

untilM :: (Monad m) => (a -> Bool) -> m a -> m a
untilM f m = do
    x <- m
    if f x then return x else untilM f m 
