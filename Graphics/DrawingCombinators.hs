--------------------------------------------------------------
-- | 
-- Module      : Graphics.DrawingCombinators
-- Copyright   : (c) Luke Palmer 2008-2010
-- License     : BSD3
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
-- For some reason the GL picking stuff ('sample') crashes GHCi, 
-- but it works okay compiled.
--------------------------------------------------------------

module Graphics.DrawingCombinators
    (
      module Graphics.DrawingCombinators.Affine
    -- * Basic types
    , Image, render, clearRender
    -- * Selection
    , sample
    -- * Initialization
    , init
    -- * Geometry
    -- 
    -- $geometry
    , point, line, regularPoly, circle, convexPoly, (%%)
    -- * Colors
    , Color(..), modulate, tint
    -- * Sprites (images from files)
    , Sprite, SpriteScaling(..), surfaceToSprite, imageToSprite, sprite
    -- * Text
    , Font, openFont, text
    )
where

import Prelude hiding (init)
import Graphics.DrawingCombinators.Affine

import Data.Maybe(fromMaybe)
import Control.Applicative (Applicative(..), liftA2, (*>))
import Control.Monad (unless, forM_)
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

type Renderer = Affine -> Color -> IO ()
type Picker a = Affine -> GL.GLuint -> IO (GL.GLuint, Set.Set GL.GLuint -> a)

-- | The type of images.
--
-- > [[Image a]] = R2 -> (Color, a)
data Image a = Image { dRender :: Renderer
                     , dPick   :: Picker a
                     }

instance Functor Image where
    fmap f d = Image { 
        dRender = dRender d,
        dPick = (fmap.fmap.fmap.fmap.fmap) f (dPick d)
      }

instance Applicative Image where
    pure x = Image { 
        dRender = (pure.pure.pure) (),
        dPick = \_ z -> pure (z, const x)
      }
    
    df <*> dx = Image {
        -- reversed so that things that come first go on top
        dRender = (liftA2.liftA2) (*>) (dRender dx) (dRender df),
        dPick = \tr z -> do
            (z', m') <- dPick dx tr z
            (z'', m) <- dPick df tr z'
            return (z'', m <*> m')
      }

instance (Monoid m) => Monoid (Image m) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- |Draw an Image on the screen in the current OpenGL coordinate
-- system (which, in absense of information, is (-1,-1) in the
-- lower left and (1,1) in the upper right).
render :: Image a -> IO ()
render d = GL.preservingAttrib [GL.AllServerAttributes] $ dRender d identity mempty

-- |Like @render@, but clears the screen first. This is so
-- you can use this module and pretend that OpenGL doesn't
-- exist at all.
clearRender :: Image a -> IO ()
clearRender d = do
    GL.clear [GL.ColorBuffer]
    render d

-- | Given a bounding box, lower left and upper right in the default coordinate
-- system (-1,-1) to (1,1), return the topmost drawing's value (with respect to
-- @`over`@) intersecting that bounding box.
selectRegion :: R2 -> R2 -> Image a -> IO a
selectRegion ll ur drawing = do
    (lookup', recs) <- GL.getHitRecords 64 $ -- XXX hard coded crap
        GL.preservingMatrix $ do
            GLU.ortho2D (fst ll) (fst ur) (snd ll) (snd ur)
            (_, lookup') <- dPick drawing identity 0
            return lookup'
    let nameList = concatMap (\(GL.HitRecord _ _ ns) -> ns) (fromMaybe [] recs)
    let nameSet  = Set.fromList $ map (\(GL.Name n) -> n) nameList
    return $ lookup' nameSet

-- | Sample the value of the image at a point.  
--
-- > [[sample p i]] = snd ([[i]] [[p]])
sample :: R2 -> Image a -> IO a
sample (px,py) = selectRegion (px-e,py-e) (px+e,py+e)
    where
    e = 1/1024


{----------------
  Initialization
----------------}

-- |Perform initialization of the library.  This can throw an exception.
init :: IO ()
init = do
    wasinit <- TTF.wasInit
    unless wasinit $ do
        success <- TTF.init
        unless success $ fail "SDL_ttf initialization failed"
        
    -- It's ok to do the GL setup here, and when rendering to not have to re-set these things each time.
    GL.texture GL.Texture2D GL.$= GL.Enabled
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- For now we assume the user wants antialiasing; the general solution is not clear - maybe let the
    -- user do the opengl setup stuff himself? otherwise need to wrap all of the possible things GL lets
    -- you set.
    GL.polygonSmooth GL.$= GL.Enabled
    GL.lineSmooth GL.$= GL.Enabled
    GL.lineWidth GL.$= 1.5
    GL.hint GL.LineSmooth GL.$= GL.DontCare



{----------------
  Geometry
-----------------}

-- $geometry
-- The geomertic combinators all return an 'Image' 'Any'.  'Any'
-- is a wrapper around 'Bool' with @(False, (||))@ as its 'Monoid'.
-- This is so you can use the 'Monoid' instance on 'Image' to
-- automatically get the union of primitives.  So:
--
-- > circle `mappend` (translate (1,0) %% circle)
--
-- Will have the value @Any True@ when /either/ of the circles is
-- sampled.  To extract the Bool, use 'getAny', or pattern match
-- on @Any True@ and @Any False@ instead of @True@ and @False@.

toVertex :: Affine -> R2 -> GL.Vertex2 GL.GLdouble
toVertex tr p = let (x,y) = tr `apply` p in GL.Vertex2 x y

inSet :: (Ord a) => a -> Set.Set a -> Any
inSet x s = Any (x `Set.member` s)

picker :: Renderer -> Picker Any
picker r tr z = z `seq` do
    GL.withName (GL.Name z) (r tr mempty)
    return (z+1, inSet z)

rendererImage :: Renderer -> Image Any
rendererImage f = Image f (picker f)

-- | A single pixel at the specified point.
--
-- > [[point p]] r | [[r]] == [[p]] = (one, Any True) 
-- >               | otherwise      = (zero, Any False)
point :: R2 -> Image Any
point p = rendererImage render'
    where
    render' tr _ = GL.renderPrimitive GL.Points . GL.vertex $ toVertex tr p

-- | A line connecting the two given points.
line :: R2 -> R2 -> Image Any
line src dest = rendererImage render'
    where
    render' tr _ = 
        GL.renderPrimitive GL.Lines $ do
            GL.vertex $ toVertex tr src
            GL.vertex $ toVertex tr dest
        

-- | A regular polygon centered at the origin with n sides.
regularPoly :: Integral a => a -> Image Any
regularPoly n = rendererImage render'
    where
    render' tr _ = do
        let scaler = 2 * pi / fromIntegral n
        GL.renderPrimitive GL.TriangleFan $ do
            GL.vertex $ toVertex tr (0,0)
            forM_ [0..n] $ \s -> do
                let theta = scaler * fromIntegral s
                GL.vertex $ toVertex tr (cos theta, sin theta)

-- | An (imperfect) unit circle centered at the origin.  Implemented as:
--
-- > circle = regularPoly 24
circle :: Image Any
circle = regularPoly (24 :: Int)

-- | A convex polygon given by the list of points.
convexPoly :: [R2] -> Image Any
convexPoly points = rendererImage render'
    where
    render' tr _ = 
        GL.renderPrimitive GL.Polygon $ 
            mapM_ (GL.vertex . toVertex tr) points

{-----------------
  Transformations
------------------}

infixr 1 %%

-- | Transform an image by an 'Affine' transformation.
--
-- > [[tr % im]] = [[im]] . inverse [[tr]]
(%%) :: Affine -> Image a -> Image a
tr' %% d = Image render' pick
    where
    render' tr col = dRender d (tr `compose` tr') col
    pick tr z = dPick d (tr `compose` tr') z


{------------
  Colors
-------------}

-- | Color is defined in the usual computer graphics sense, of 
-- a 4 vector containing red, green, blue, and alpha.
--
-- The Monoid instance is given by alpha transparency blending,
-- so:
--
-- > mempty = Color 1 1 1 1
-- > mappend c@(Color _ _ _ a) c'@(Color _ _ _ a') = a*c + (1-a)*c'
--
-- Where multiplication is componentwise.  In the semantcs the
-- values @zero@ and @one@ are used, which are defined as:
--
-- > zero = Color 0 0 0 0
-- > one = Color 1 1 1 1
data Color = Color R R R R

instance Monoid Color where
    mempty = Color 1 1 1 1
    mappend (Color r g b a) (Color r' g' b' a') = Color (i r r') (i g g') (i b b') (i a a')
        where
        i x y = a*x + (1-a)*y

-- | Modulate two colors by each other.
--
-- > modulate (Color r g b a) (Color r' g' b' a') 
-- >           = Color (r*r') (g*g') (b*b') (a*a')
modulate :: Color -> Color -> Color
modulate (Color r g b a) (Color r' g' b' a') = Color (r*r') (g*g') (b*b') (a*a')

-- | Tint an image by a color; i.e. modulate the colors of an image by 
-- a color.
--
-- > [[tint c im]] = first (modulate c) . [[im]]
-- >    where first f (x,y) = (f x, y)
tint :: Color -> Image a -> Image a
tint c d = Image render' (dPick d)
    where
    render' tr col = do
        let oldColor = col
            newColor = modulate c col
        setColor newColor
        result <- dRender d tr newColor
        setColor oldColor
        return result
    setColor (Color r g b a) = GL.color $ GL.Color4 r g b a


{-------------------------
  Sprites (bitmap images)
-------------------------}

-- | A Sprite represents a bitmap image.
--
-- > [[Sprite]] = [-1,1]^2 -> Color
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

-- | Indicate how a non-square image is to be mapped to a sprite.
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
                        _ -> error "Unknown pixel format"
    GL.textureFunction GL.$= GL.Modulate
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.Repeat)
    GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA'  -- ? proxy level internalformat
                  (GL.TextureSize2D 
                    (fromIntegral $ SDL.surfaceGetWidth surf')
                    (fromIntegral $ SDL.surfaceGetHeight surf'))
                  0 -- border
                  (GL.PixelData pixelFormat GL.UnsignedByte pixels)
    GL.textureBinding GL.Texture2D GL.$= oldtex
    let (w,w') = (SDL.surfaceGetWidth  surf, SDL.surfaceGetWidth  surf')
        (h,h') = (SDL.surfaceGetHeight surf, SDL.surfaceGetHeight surf')
    let (scalew, scaleh) = scaleFunc w h
    let sprite' = Sprite { spriteObject = obj
                         , spriteWidthRat  = fromIntegral w / fromIntegral w'
                         , spriteHeightRat = fromIntegral h / fromIntegral h'
                         , spriteWidth  = scalew
                         , spriteHeight = scaleh
                         }
                            
    addFinalizer sprite' $ freeTexture obj
    return sprite'

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

nextPowerOf2 :: (Ord a, Num a) => a -> a
nextPowerOf2 x = head $ dropWhile (< x) $ iterate (*2) 1


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

-- | The image of a sprite at the origin.
--
-- > [[sprite s]] p | p `elem` [-1,1]^2 = ([[s]] p, Any True) 
-- >                | otherwise         = (zero, Any False)
sprite :: Sprite -> Image Any
sprite spr = rendererImage render'
    where
    render' tr _ = do
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

-- | Load a TTF font from a file with the given point size (higher numbers
-- mean smoother text but more expensive rendering).
openFont :: String -> Int -> IO Font
openFont path res = do
    font <- TTF.openFont path res
    let font' = Font font
    return font'

textSprite :: Font -> String -> IO Sprite
textSprite font str = do
    surf <- TTF.renderTextBlended (getFont font) str (SDL.Color 255 255 255)
    surfaceToSprite ScaleHeight surf

-- | The image representing some text rendered with a font.  The resulting 
-- string will have height 1.
text :: Font -> String -> Image Any
text font s = sprite $ unsafePerformIO $ textSprite font s
