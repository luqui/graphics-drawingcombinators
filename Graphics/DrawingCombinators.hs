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
      module Graphics.DrawingCombinators.Affine
    -- * Basic types
    , Draw, render, clearRender
    -- * Selection
    , sample
    -- * Initialization
    , init
    -- * Geometric Primitives
    , point, line, regularPoly, circle, convexPoly
    -- * Transformation
    , (%%)
    -- * Colors 
    , Color, color, colorFunc
    -- * Sprites (images from files)
    , Sprite, SpriteScaling(..), surfaceToSprite, imageToSprite, sprite
    -- * Text
    , Font, openFont, text
    )
where

import Prelude hiding (init)
import Graphics.DrawingCombinators.Affine
import Control.Applicative (Applicative(..), liftA2, (*>))
import Control.Monad (when, forM_)
import Data.Monoid (Monoid(..), Any(..))
import System.Mem.Weak (addFinalizer)
import qualified Data.Set as Set
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import qualified Graphics.UI.SDL.TTF as TTF
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import System.IO.Unsafe (unsafePerformIO)  -- for hacking around OpenGL bug :-(

type Color = (R,R,R,R)
type ColorT = Color -> Color

type Renderer = Affine -> ColorT -> IO ()
type Picker a = Affine -> GL.GLuint -> IO (GL.GLuint, Set.Set GL.GLuint -> a)

data Draw a = Draw { dRender :: Renderer
                   , dPick   :: Picker a
                   }

instance Functor Draw where
    fmap f d = Draw { 
        dRender = dRender d,
        dPick = (fmap.fmap.fmap.fmap.fmap) f (dPick d)
      }

instance Applicative Draw where
    pure x = Draw { 
        dRender = (pure.pure.pure) (),
        dPick = \tr z -> pure (z, const x)
      }
    
    df <*> dx = Draw {
        dRender = (liftA2.liftA2) (*>) (dRender df) (dRender dx),
        dPick = \tr z -> do
            (z', m) <- dPick df tr z
            (z'', m') <- dPick dx tr z'
            return (z'', m <*> m')
      }

instance (Monoid m) => Monoid (Draw m) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- |Draw a Drawing on the screen in the current OpenGL coordinate
-- system (which, in absense of information, is (-1,-1) in the
-- lower left and (1,1) in the upper right).
render :: Draw a -> IO ()
render d = do
    GL.preservingAttrib [GL.AllServerAttributes] $ do
        GL.texture GL.Texture2D GL.$= GL.Enabled
        GL.blend GL.$= GL.Enabled
        GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.depthFunc GL.$= Nothing
        dRender d identity id

-- |Like @render@, but clears the screen first. This is so
-- you can use this module and pretend that OpenGL doesn't
-- exist at all.
clearRender :: Draw a -> IO ()
clearRender d = do
    GL.clear [GL.ColorBuffer]
    render d

-- | Given a bounding box, lower left and upper right in the default coordinate
-- system (-1,-1) to (1,1), return the topmost drawing's value (with respect to
-- @`over`@) intersecting that bounding box.
selectRegion :: Vec2 -> Vec2 -> Draw a -> IO a
selectRegion ll ur drawing = do
    (lookup, recs) <- GL.getHitRecords 64 $ do -- XXX hard coded crap
        GL.preservingMatrix $ do
            GLU.ortho2D (fst ll) (fst ur) (snd ll) (snd ur)
            (_, lookup) <- dPick drawing identity 0
            return lookup
    let nameList = concatMap (\(GL.HitRecord _ _ ns) -> ns) (maybe [] id recs)
    let nameSet  = Set.fromList $ map (\(GL.Name n) -> n) nameList
    return $ lookup nameSet

sample :: Vec2 -> Draw a -> IO a
sample (px,py) = selectRegion (px-e,py-e) (px+e,py+e)
    where
    e = 1/1024


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

toVertex :: Affine -> Vec2 -> GL.Vertex2 GL.GLdouble
toVertex tr p = let (x,y) = tr `apply` p in GL.Vertex2 x y

inSet :: (Ord a) => a -> Set.Set a -> Any
inSet x s = Any (x `Set.member` s)

picker :: Renderer -> Picker Any
picker r tr z = z `seq` do
    GL.withName (GL.Name z) (r tr id)
    return (z+1, inSet z)

-- | Draw a single pixel at the specified point.
point :: Vec2 -> Draw Any
point p = Draw render (picker render)
    where
    render tr colt = GL.renderPrimitive GL.Points . GL.vertex $ toVertex tr p

-- | Draw a line connecting the two given points.
line :: Vec2 -> Vec2 -> Draw Any
line src dest = Draw render (picker render)
    where
    render tr colt = 
        GL.renderPrimitive GL.Lines $ do
            GL.vertex $ toVertex tr src
            GL.vertex $ toVertex tr dest
        

-- | Draw a regular polygon centered at the origin with n sides.
regularPoly :: Int -> Draw Any
regularPoly n = Draw render (picker render)
    where
    render tr colt = do
        let scaler = 2 * pi / fromIntegral n
        GL.renderPrimitive GL.TriangleFan $ do
            GL.vertex $ toVertex tr (0,0)
            forM_ [0..n] $ \s -> do
                let theta = scaler * fromIntegral s
                GL.vertex $ toVertex tr (cos theta, sin theta)

-- | Draw a unit circle centered at the origin.  This is equivalent
-- to @regularPoly 24@.
circle :: Draw Any
circle = regularPoly 24

-- | Draw a convex polygon given by the list of points.
convexPoly :: [Vec2] -> Draw Any
convexPoly points = Draw render (picker render)
    where
    render tr colt = 
        GL.renderPrimitive GL.Polygon $ 
            mapM_ (GL.vertex . toVertex tr) points

{-----------------
  Transformations
------------------}

(%%) :: Affine -> Draw a -> Draw a
tr' %% d = Draw render pick
    where
    render tr colt = dRender d (tr' `compose` tr) colt
    pick tr z = dPick d (tr' `compose` tr) z


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
colorFunc colt' d = Draw render (dPick d)
    where
    render tr colt = do
        let oldcolor = colt (1,1,1,1)
            newcolor = colt' oldcolor
        setColor newcolor
        result <- dRender d tr (colt' . colt)
        setColor oldcolor
        return result
    setColor (r,g,b,a) = GL.color $ GL.Color4 r g b a

-- | @color c d@ sets the color of the drawing to exactly @c@.
color :: Color -> Draw a -> Draw a
color c = colorFunc (const c)


{-------------------------
  Sprites (bitmap images)
-------------------------}

-- | A sprite represents a bitmap image.
data Sprite = Sprite { spriteObject :: GL.TextureObject
                     , spriteWidthRat :: R
                     , spriteHeightRat :: R
                     , spriteWidth :: R
                     , spriteHeight :: R
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
    atomicModifyIORef textureHack (\xs -> (b:xs,()))

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
sprite :: Sprite -> Draw Any
sprite spr = Draw render (picker render)
    where
    render tr colt = do
        oldtex <- GL.get (GL.textureBinding GL.Texture2D)
        GL.textureBinding GL.Texture2D GL.$= (Just $ spriteObject spr)
        GL.renderPrimitive GL.Quads $ do
            let (xofs, yofs) = (0.5 * spriteWidth spr, 0.5 * spriteHeight spr)
                (xrat, yrat) = (spriteWidthRat spr, spriteHeightRat spr)
            GL.texCoord $ GL.TexCoord2 0 (0 :: GL.GLdouble)
            GL.vertex   $ toVertex tr (-xofs, yofs)
            GL.texCoord $ GL.TexCoord2 xrat 0
            GL.vertex   $ toVertex tr (xofs, yofs)
            GL.texCoord $ GL.TexCoord2 xrat yrat
            GL.vertex   $ toVertex tr (xofs,-yofs)
            GL.texCoord $ GL.TexCoord2 0 yrat
            GL.vertex   $ toVertex tr (-xofs,-yofs)
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
text :: Font -> String -> Draw Any
text font str = sprite $ unsafePerformIO $ textSprite font str
