{-# LANGUAGE GADTs, RankNTypes #-}

--------------------------------------------------------------
-- | 
-- Module      : Graphics.DrawingCombinators
-- Copyright   : (c) Luke Palmer 2008
-- License     : LGPL
--
-- Maintainer  : Luke Palmer <lrpalmer@gmail.com>
-- Stability   : experimental
-- Portability : needs GADTs and rank n types
--
-- Drawing combinators as a functional interface to OpenGL
-- (for 2D drawings only... for now).
--
-- This module is intended to be imported @qualified@, as in:
--
-- > import Graphics.DrawingCombinators as Draw
--
-- It is recommended that you use this module in combination
-- with SDL; it has not been tested in any other environments.
-- For some reason the selection stuff ("selectRegion", "click")
-- crashes GHCi, but it works okay compiled.
--------------------------------------------------------------

module Graphics.DrawingCombinators
    (
    -- * Basic types
      Draw, runDrawing, draw, Vec2
    -- * Selection
    , selectRegion, click
    -- * Combinators
    , over, overlay, empty
    -- * Initialization
    , init
    -- * Geometric Primitives
    , point, line, regularPoly, circle, convexPoly
    -- * Transformations
    , translate, rotate, scale
    -- * Colors 
    , Color, color, colorFunc
    -- * Sprites (images from files)
    , Sprite, SpriteScaling(..), surfaceToSprite, imageToSprite, sprite
    -- * Text
    , Font, openFont, text
    )
where

import Prelude hiding (init)
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import qualified Graphics.UI.SDL.TTF as TTF
import System.Mem.Weak
import Data.IORef 
import System.IO.Unsafe
import qualified Data.Set as Set
import Debug.Trace

type Vec2 = (Double,Double)
type Color = (Double,Double,Double,Double)

type DrawM a = ReaderT DrawCxt IO a

-- | @Draw a@ represents a drawing which returns a value of type
-- a when selected.
data Draw a where
    DrawGL      :: DrawM () -> Draw ()
    TransformGL :: (forall x. DrawM x -> DrawM x) -> Draw a -> Draw a
    Empty       :: Draw a
    Over        :: (a -> a -> a) -> Draw a -> Draw a -> Draw a
    FMap        :: (a -> b) -> Draw a -> Draw b

-- |Draw a Drawing on the screen in the current OpenGL coordinate
-- system (which, in absense of information, is (-1,-1) in the
-- lower left and (1,1) in the upper right).
runDrawing :: Draw a -> IO ()
runDrawing d = runReaderT (run' d) initDrawCxt
    where
    run' :: Draw a -> DrawM ()
    run' (DrawGL m) = m
    run' (TransformGL f m) = f (run' m)
    run' Empty = return ()
    run' (Over f a b) = run' b >> run' a
    run' (FMap f d) = run' d

-- |Like runDrawing, but clears the screen first.  This is so
-- you can use this module and pretend that OpenGL doesn't
-- exist at all.
draw :: Draw a -> IO ()
draw d = do
    GL.clear [GL.ColorBuffer]
    runDrawing d

-- | Given a bounding box, lower left and upper right in the default coordinate
-- system (-1,-1) to (1,1), return the topmost drawing's value (with respect to
-- @`over`@) intersecting that bounding box.
selectRegion :: Vec2 -> Vec2 -> Draw a -> IO (Maybe a)
selectRegion ll ur drawing = do
    ((), recs) <- GL.getHitRecords 64 $ do -- XXX hard coded crap
        GL.preservingMatrix $ do
            GLU.ortho2D (fst ll) (fst ur) (snd ll) (snd ur)
            runReaderT (draw' 0 drawing) initDrawCxt
            return ()
    let nameList = concatMap (\(GL.HitRecord _ _ ns) -> ns) (maybe [] id recs)
    let nameSet  = Set.fromList $ map (\(GL.Name n) -> n) nameList
    return $ fst $ lookupName 0 nameSet drawing
    where
    draw' :: GL.GLuint -> Draw a -> DrawM GL.GLuint
    draw' n (DrawGL m) = do
        r <- ask
        lift $ GL.withName (GL.Name n) $ runReaderT m r
        return $! n+1
    draw' n (TransformGL f m) = f (draw' n m)
    draw' n Empty = return n
    draw' n (Over f a b) = do
        n' <- draw' n b
        draw' n' a
    draw' n (FMap f d) = draw' n d
    
    lookupName :: GL.GLuint -> Set.Set GL.GLuint -> Draw a -> (Maybe a, GL.GLuint)
    lookupName n names (DrawGL m)
        | n `Set.member` names = (Just (), n + 1)
        | otherwise            = (Nothing, n + 1)
    lookupName n names (TransformGL _ m) = lookupName n names m
    lookupName n names Empty = (Nothing, n)
    lookupName n names (Over f a b) =
        let (lb, n')  = lookupName n  names b
            (la, n'') = lookupName n' names a
        in (joinMaybes f la lb, n'')
    lookupName n names (FMap f d) = 
        let (l, n') = lookupName n names d
        in (fmap f l, n')

joinMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinMaybes f Nothing x = x
joinMaybes f x Nothing = x
joinMaybes f (Just x) (Just y) = Just (f x y)

click :: Vec2 -> Draw a -> IO (Maybe a)
click (px,py) = selectRegion (px-e,py-e) (px+e,py+e)
    where
    e = 1/1024

data DrawCxt 
    = DrawCxt { colorTrans :: Color -> Color }

initDrawCxt = DrawCxt { colorTrans = id }

over :: Draw a -> Draw a -> Draw a
over = overlay const

overlay :: (a -> a -> a) -> Draw a -> Draw a -> Draw a
overlay = Over

empty :: Draw a
empty = Empty

instance Functor Draw where
    fmap = FMap

instance (Monoid a) => Monoid (Draw a) where
    mempty = empty
    mappend = overlay mappend


{----------------
  Initialization
----------------}

-- |Perform initialization of the library.  This can fail.
init :: IO ()
init = do
    wasinit <- TTF.wasInit
    when (not wasinit) $ do
        success <- TTF.init
        when (not success) $ fail "SDL_ttf initialization failed"


{----------------
  Geometric Primitives
-----------------}

-- | Draw a single pixel at the specified point.
point :: Vec2 -> Draw ()
point (ax,ay) = DrawGL $ lift $
    GL.renderPrimitive GL.Points $
        GL.vertex $ GL.Vertex2 ax ay

-- | Draw a line connecting the two given points.
line :: Vec2 -> Vec2 -> Draw ()
line (ax,ay) (bx,by) = DrawGL $ lift $ 
    GL.renderPrimitive GL.Lines $ do
        GL.vertex $ GL.Vertex2 ax ay
        GL.vertex $ GL.Vertex2 bx by

-- | Draw a regular polygon centered at the origin with n sides.
regularPoly :: Int -> Draw ()
regularPoly n = DrawGL $ lift $ do
    let scaler = 2 * pi / fromIntegral n :: Double
    GL.renderPrimitive GL.TriangleFan $ do
        GL.vertex $ (GL.Vertex2 0 0 :: GL.Vertex2 Double)
        forM_ [0..n] $ \s -> do
            let theta = scaler * fromIntegral s
            GL.vertex $ GL.Vertex2 (cos theta) (sin theta)

-- | Draw a unit circle centered at the origin.  This is equivalent
-- to @regularPoly 24@.
circle :: Draw ()
circle = regularPoly 24

-- | Draw a convex polygon given by the list of points.
convexPoly :: [Vec2] -> Draw ()
convexPoly points = DrawGL $ lift $ do
    GL.renderPrimitive GL.Polygon $ do
        forM_ points $ \(x,y) -> do
            GL.vertex $ GL.Vertex2 x y

{-----------------
  Transformations
------------------}

-- | Translate the given drawing by the given amount.
translate :: Vec2 -> Draw a -> Draw a
translate (byx,byy) = TransformGL $ \d -> do
    r <- ask
    lift $ GL.preservingMatrix $ do
        GL.translate (GL.Vector3 byx byy 0)
        runReaderT d r

-- | Rotate the given drawing counterclockwise by the
-- given number of radians.
rotate :: Double -> Draw a -> Draw a
rotate rad = TransformGL $ \d -> do
    r <- ask
    lift $ GL.preservingMatrix $ do
        GL.rotate (180 * rad / pi) (GL.Vector3 0 0 1)
        runReaderT d r

-- | @scale x y d@ scales @d@ by a factor of @x@ in the
-- horizontal direction and @y@ in the vertical direction.
scale :: Double -> Double -> Draw a -> Draw a
scale x y = TransformGL $ \d -> do
    r <- ask
    lift $ GL.preservingMatrix $ do
        GL.scale x y 1
        runReaderT d r

{------------
  Colors
-------------}

-- | @colorFunc f d@ modifies all colors appearing in @d@ with
-- the function @f@.  For example:
--
-- > colorFunc (\(r,g,b,a) -> (r,g,b,a/2)) d
--
-- Will draw d at greater transparency, regardless of the calls
-- to color within.
colorFunc :: (Color -> Color) -> Draw a -> Draw a
colorFunc cf = TransformGL $ \d -> do
    r <- ask
    let trans    = colorTrans r
        newtrans = trans . cf
        oldcolor = trans (1,1,1,1)
        newcolor = newtrans (1,1,1,1)
    setColor newcolor
    result <- local (const (r { colorTrans = newtrans })) d
    setColor oldcolor
    return result
    where
    setColor (r,g,b,a) = lift $ GL.color $ GL.Color4 r g b a

-- | @color c d@ sets the color of the drawing to exactly @c@.
color :: Color -> Draw a -> Draw a
color c = colorFunc (const c)


{-------------------------
  Sprites (bitmap images)
-------------------------}

-- | A sprite represents a bitmap image.
data Sprite = Sprite { spriteObject :: GL.TextureObject
                     , spriteWidthRat :: Double
                     , spriteHeightRat :: Double
                     , spriteWidth :: Double
                     , spriteHeight :: Double
                     }

-- FUUUUUUUUUCKKK Why doesn't glGenTextures work!!??
-- Anyway here is me hacking around it...
textureHack :: IORef [GL.GLuint]
textureHack = unsafePerformIO $ newIORef [1..]

allocateTexture :: IO GL.TextureObject
allocateTexture = do
    {- -- This is how it *should* be done.  wtf is going on!?
    [obj] <- GL.genObjectNames 1
    good <- GL.isObjectName obj
    unless good $ fail "Failed to generate valid object wtf!"
    return obj
    -}
    b <- atomicModifyIORef textureHack (\(x:xs) -> (xs,x))
    return $ GL.TextureObject b

freeTexture :: GL.TextureObject -> IO ()
freeTexture (GL.TextureObject b) = do
    GL.deleteObjectNames [GL.TextureObject b]
    modifyIORef textureHack (b:)

-- | Indicate how a nonrectangular image is to be mapped to a sprite.
data SpriteScaling
    -- | ScaleMax will set the maximum of the height and width of the image to 1.
    = ScaleMax    
    -- | ScaleWidth will set the width of the image to 1, and scale the height appropriately.
    | ScaleWidth  
    -- | ScaleHeight will set the height of the image to 1, and scale the width appropriately. 
    | ScaleHeight

-- | Convert an SDL.Surface to a Sprite.
surfaceToSprite :: SpriteScaling -> SDL.Surface -> IO Sprite
surfaceToSprite scaling surf = do
    surf' <- padSurface surf
    obj <- allocateTexture
    oldtex <- GL.get (GL.textureBinding GL.Texture2D)
    GL.textureBinding GL.Texture2D GL.$= Just obj
    pixels <- SDL.surfaceGetPixels surf'
    bytesPerPixel <- SDL.pixelFormatGetBytesPerPixel (SDL.surfaceGetPixelFormat surf')
    let pixelFormat = case bytesPerPixel of
                        3 -> GL.RGB
                        4 -> GL.RGBA
    GL.textureFunction GL.$= GL.Modulate
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.Repeat)
    GL.texImage2D Nothing GL.NoProxy 0 (GL.RGBA')  -- ? proxy level internalformat
                  (GL.TextureSize2D 
                    (fromIntegral $ SDL.surfaceGetWidth surf')
                    (fromIntegral $ SDL.surfaceGetHeight surf'))
                  0 -- border
                  (GL.PixelData pixelFormat GL.UnsignedByte pixels)
    GL.textureBinding GL.Texture2D GL.$= oldtex
    let (w,w') = (SDL.surfaceGetWidth  surf, SDL.surfaceGetWidth  surf')
        (h,h') = (SDL.surfaceGetHeight surf, SDL.surfaceGetHeight surf')
    let (scalew, scaleh) = scaleFunc w h
    let sprite = Sprite { spriteObject = obj
                        , spriteWidthRat  = fromIntegral w / fromIntegral w'
                        , spriteHeightRat = fromIntegral h / fromIntegral h'
                        , spriteWidth  = scalew
                        , spriteHeight = scaleh
                        }
                            
    addFinalizer sprite $ do
        freeTexture obj
    return sprite

    where
    scaleFunc w h =
        case scaling of
             ScaleMax ->
                 ( fromIntegral w / fromIntegral (max w h)
                 , fromIntegral h / fromIntegral (max w h) )
             ScaleWidth ->
                 ( 1, fromIntegral h / fromIntegral w )
             ScaleHeight ->
                 ( fromIntegral w / fromIntegral h, 1 )

nextPowerOf2 x = head $ dropWhile (< x) $ iterate (*2) 1
isPowerOf2 x = x == nextPowerOf2 x

padSurface :: SDL.Surface -> IO SDL.Surface
padSurface surf 
    | newWidth == oldWidth && newHeight == oldHeight = return surf
    | otherwise = do
        surf' <- SDL.createRGBSurfaceEndian [] newWidth newHeight 32
        SDL.setAlpha surf [] 0xff
        SDL.blitSurface surf Nothing surf' Nothing
        return surf'
    where
    oldWidth  = SDL.surfaceGetWidth surf
    oldHeight = SDL.surfaceGetHeight surf
    newWidth  = nextPowerOf2 oldWidth
    newHeight = nextPowerOf2 oldHeight

-- | Load an image from a file and create a sprite out of it.
imageToSprite :: SpriteScaling -> FilePath -> IO Sprite
imageToSprite scaling path = Image.load path >>= surfaceToSprite scaling

-- | Draw a sprite at the origin.
sprite :: Sprite -> Draw ()
sprite spr = DrawGL $ liftIO $ do
    oldtex <- GL.get (GL.textureBinding GL.Texture2D)
    GL.textureBinding GL.Texture2D GL.$= (Just $ spriteObject spr)
    GL.renderPrimitive GL.Quads $ do
        let (xofs, yofs) = (0.5 * spriteWidth spr, 0.5 * spriteHeight spr)
            (xrat, yrat) = (spriteWidthRat spr, spriteHeightRat spr)
        GL.texCoord $ GL.TexCoord2 0 (0 :: Double)
        GL.vertex   $ GL.Vertex2 (-xofs) yofs
        GL.texCoord $ GL.TexCoord2 xrat 0
        GL.vertex   $ GL.Vertex2 xofs yofs
        GL.texCoord $ GL.TexCoord2 xrat yrat
        GL.vertex   $ GL.Vertex2 xofs (-yofs)
        GL.texCoord $ GL.TexCoord2 0 yrat
        GL.vertex   $ GL.Vertex2 (-xofs) (-yofs)
    GL.textureBinding GL.Texture2D GL.$= oldtex

{---------
 Text
---------}

data Font = Font { getFont :: TTF.Font }

-- | Load a TTF font from a file.
openFont :: String -> Int -> IO Font
openFont path res = do
    font <- TTF.openFont path res
    let font' = Font font
    return font'

textSprite :: Font -> String -> IO Sprite
textSprite font str = do
    surf <- TTF.renderTextBlended (getFont font) str (SDL.Color 255 255 255)
    surfaceToSprite ScaleHeight surf

-- | Draw a string using a font.  The resulting string will have height 1.
text :: Font -> String -> Draw ()
text font str = sprite $ unsafePerformIO $ textSprite font str
