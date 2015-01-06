{-# LANGUAGE CPP #-}

--------------------------------------------------------------
-- |
-- Module      : Graphics.DrawingCombinators
-- Copyright   : (c) Luke Palmer 2008-2010
-- License     : BSD3
--
-- Maintainer  : Luke Palmer <lrpalmer@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Drawing combinators as a functional interface to 2D graphics using OpenGL.
--
-- This module is intended to be imported @qualified@, as in:
--
-- > import qualified Graphics.DrawingCombinators as Draw
--
-- Whenever possible, a /denotational semantics/ for operations in this library
-- is given.  Read @[[x]]@ as \"the meaning of @x@\".
--
-- Intuitively, an 'Image' @a@ is an infinite plane of pairs of colors /and/
-- @a@\'s.  The colors are what are drawn on the screen when you 'render', and
-- the @a@\'s are what you can recover from coordinates using 'sample'.  The
-- latter allows you to tell, for example, what a user clicked on.
--
-- The following discussion is about the associated data.  If you are only
-- interested in drawing, rather than mapping from coordinates to values, you
-- can ignore the following and just use 'mappend' and 'mconcat' to overlay images.
--
-- Wrangling the @a@\'s -- the associated data with each \"pixel\" -- is done
-- using the 'Functor', 'Applicative', and 'Monoid' instances.
--
-- The primitive @Image@s such as 'circle' and 'text' all return @Image Any@
-- objects.  'Any' is just a wrapper around 'Bool', with @(||)@ as its monoid
-- operator.  So e.g. the points inside the circle will have the value @Any
-- True@, and those outside will have the value @Any False@.  Returning @Any@
-- instead of plain @Bool@ allows you to use @Image@s as a monoid, e.g.
-- 'mappend' to overlay two images. But if you are doing anything with
-- sampling, you probably want to map this to something.  Here is a drawing
-- with two circles that reports which one was hit:
--
-- > twoCircles :: Image String
-- > twoCircles = liftA2 test (translate (-1,0) %% circle) (translate (1,0) %% circle)
-- >   where
-- >   test (Any False) (Any False) = "Miss!"
-- >   test (Any False) (Any True)  = "Hit Right!"
-- >   test (Any True)  (Any False) = "Hit Left!"
-- >   test (Any True)  (Any True)  = "Hit Both??!"
--
-- The last case would only be possible if the circles were overlapping.
--
-- Note, the area-less shapes such as 'point', 'line', and 'bezierCurve'
-- /always/ return @Any False@ when sampled, even if the exact same
-- coordinates are given.  This is because miniscule floating-point error
-- can make these shapes very brittle under transformations.  If you need
-- a point to be clickable, make it, for example, a very small box.
--------------------------------------------------------------

module Graphics.DrawingCombinators
    (
      module Graphics.DrawingCombinators.Affine
    -- * Basic types
    , Image
    , render, clearRender
    , renderSized, clearRenderSized
    -- * Selection
    , sample
    -- * Geometry
    , point, line, regularPoly, circle, convexPoly, (%%), bezierCurve
    -- * Colors
    , Color(..), modulate, tint
    -- * Sprites (images from files)
    , Sprite, openSprite, sprite
    -- * Text
    , Font, openFont, text, textWidth, withFont
    -- * Extensions
    , unsafeOpenGLImage
    , Monoid(..), Any(..)
    )
where

import Graphics.DrawingCombinators.Affine
import Control.Applicative (Applicative(..), liftA2, (<$>))
import qualified Control.Exception as Exception
import Data.Monoid (Monoid(..), Any(..))
import qualified Data.Bitmap.OpenGL as Bitmap
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Codec.Image.STB as Image
import System.IO.Unsafe (unsafePerformIO)  -- for pure textWidth
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.List (minimumBy)
import Data.Ord (comparing)

#ifdef LAME_FONTS
import qualified Graphics.UI.GLUT as GLUT
import Control.Monad (unless)
#else
import qualified Graphics.Rendering.FTGL as FTGL
import System.Mem.Weak (addFinalizer)
#endif

type GLPixelRatio = R2

type Renderer = GLPixelRatio -> Affine -> Color -> IO ()
type Picker a = R2 -> a

-- | The type of images.
--
-- > [[Image a]] = R2 -> (Color, a)
--
-- The semantics of the instances are all consistent with /type class morphism/.
-- I.e. Functor, Applicative, and Monoid act point-wise, using the 'Color' monoid
-- described below.
data Image a = Image { dRender :: Renderer
                     , dPick   :: Picker a
                     }

instance Functor Image where
    fmap f d = Image {
        dRender = dRender d,
        dPick = fmap f (dPick d)
      }

instance Applicative Image where
    pure x = Image {
        dRender = (pure.pure.pure.pure) (),
        dPick = const x
      }

    df <*> dx = Image {
        -- reversed so that things that come first go on top
        dRender = (liftA2.liftA2.liftA2.liftA2) mappend (dRender dx) (dRender df),
        dPick = dPick df <*> dPick dx
      }

instance (Monoid m) => Monoid (Image m) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- |Like `render', but lets you to specify the size of a single GL unit 
renderSized :: R2 -> Image a -> IO ()
renderSized glPixelRatio d =
    GL.preservingAttrib [GL.AllServerAttributes] $ do
        GL.blend GL.$= GL.Enabled
        GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        -- For now we assume the user wants antialiasing; the general solution is not clear - maybe let the
        -- user do the opengl setup stuff himself? otherwise need to wrap all of the possible things GL lets
        -- you set.
        GL.polygonSmooth GL.$= GL.Enabled
        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 1.5
        GL.hint GL.LineSmooth GL.$= GL.DontCare

        dRender d glPixelRatio identity white

-- |Draw an Image on the screen in the current OpenGL coordinate
-- system (which, in absense of information, is (-1,-1) in the
-- lower left and (1,1) in the upper right).
render :: Image a -> IO ()
render =
    -- Arbitrary approximation of a GL unit size. Getting this wrong
    -- means font rendering will be of slightly lesser quality
    renderSized (500, 500)

-- |Like 'render', but clears the screen first. This is so
-- you can use this module and pretend that OpenGL doesn't
-- exist at all.
clearRender :: Image a -> IO ()
clearRender d = do
    GL.clear [GL.ColorBuffer]
    render d

-- |Like 'clearRender', but for 'renderSized'.
clearRenderSized :: R2 -> Image a -> IO ()
clearRenderSized glPixelRatio d = do
    GL.clear [GL.ColorBuffer]
    renderSized glPixelRatio d

-- | Sample the value of the image at a point.
--
-- > [[sample i p]] = snd ([[i]] p)
sample :: Image a -> R2 -> a
sample = dPick

{----------------
  Geometry
-----------------}

toVertex :: Affine -> R2 -> GL.Vertex2 GL.GLdouble
toVertex tr p = let (x,y) = tr `apply` p in GL.Vertex2 x y

toVertex3 :: R -> Affine -> R2 -> GL.Vertex3 GL.GLdouble
toVertex3 z tr p = let (x,y) = tr `apply` p in GL.Vertex3 x y z

-- | A single \"pixel\" at the specified point.
--
-- > [[point p]] r | [[r]] == [[p]] = (one, Any True)
-- >               | otherwise      = (zero, Any False)
point :: R2 -> Image Any
point p = Image render' (const (Any False))
    where
    render' _glPixelRatio tr _ =
        withoutTextures . GL.renderPrimitive GL.Points . GL.vertex $ toVertex tr p

withoutTextures :: IO a -> IO a
withoutTextures action =
    GL.texture GL.Texture2D GL.$= GL.Disabled >> action

-- | A line connecting the two given points.
line :: R2 -> R2 -> Image Any
line src dest = Image render' (const (Any False))
    where
    render' _ tr _ = withoutTextures . GL.renderPrimitive GL.Lines $ do
        GL.vertex $ toVertex tr src
        GL.vertex $ toVertex tr dest


-- | A regular polygon centered at the origin with n sides.
regularPoly :: Int -> Image Any
regularPoly n = convexPoly [ (cos theta, sin theta) | i <- [0..n-1], let theta = fromIntegral i * (2 * pi / fromIntegral n) ]

-- | An (imperfect) unit circle centered at the origin.  Implemented as:
--
-- > circle = regularPoly 24
circle :: Image Any
circle = regularPoly 24

-- | A convex polygon given by the list of points.
convexPoly :: [R2] -> Image Any
convexPoly points@(_:_:_:_) = Image render' pick
    where
    render' _ tr _ =
        withoutTextures . GL.renderPrimitive GL.Polygon $ mapM_ (GL.vertex . toVertex tr) points
    pick p = Any $ all (sign . side p) edges
        where
        edges = zipWith (,) points (tail points)
        side (x,y) ((x1,y1), (x2,y2)) = (y-y1)*(x2-x1) - (x-x1)*(y2-y1)
        sign | side p (last points, head points) >= 0 = (>= 0)
             | otherwise                              = (<= 0)
convexPoly _ = error "convexPoly must be given at least three points"

-- | A Bezier curve given a list of control points.  It is a curve
-- that begins at the first point in the list, ends at the last one,
-- and smoothly interpolates between the rest.  It is the empty
-- image ('mempty') if zero or one points are given.
bezierCurve :: [R2] -> Image Any
bezierCurve controlPoints = Image render' (const (Any False))
    where
    render' _ tr _ = do
        let ps = map (toVertex3 0 tr) controlPoints
        m <- GL.newMap1 (0,1) ps :: IO (GL.GLmap1 (GL.Vertex3) R)
        GL.map1 GL.$= Just m
        GL.mapGrid1 GL.$= (100, (0::R, 1))
        GL.evalMesh1 GL.Line (1,100)

{-----------------
  Transformations
------------------}

infixr 1 %%

-- | Transform an image by an 'Affine' transformation.
--
-- > [[tr % im]] = [[im]] . inverse [[tr]]
(%%) :: Affine -> Image a -> Image a
tr' %% d = tr' `seq` Image render' pick
    where
    render' glPixelRatio tr col = dRender d glPixelRatio (tr `compose` tr') col
    pick = dPick d . apply (inverse tr')


{------------
  Colors
-------------}

-- | Color is defined in the usual computer graphics sense:
-- a 4 vector containing red, green, blue, and alpha.
--
-- The Monoid instance is given by alpha composition, described
-- at @http:\/\/lukepalmer.wordpress.com\/2010\/02\/05\/associative-alpha-blending\/@
--
-- In the semantcs the values @zero@ and @one@ are used, which are defined as:
--
-- > zero = Color 0 0 0 0
-- > one = Color 1 1 1 1
data Color = Color !R !R !R !R
    deriving (Eq,Show)

instance Monoid Color where
    mempty = Color 0 0 0 0
    mappend (Color r g b a) (Color r' g' b' a') = Color (i r r') (i g g') (i b b') γ
        where
        γ = a + a' - a * a'
        i | γ == 0    = \_ _ -> 0  -- imples a = a' = 0
          | otherwise = \x y -> (a*x + (1-a)*a'*y)/γ

white :: Color
white = Color 1 1 1 1

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
    render' glPixelRatio tr col = do
        let oldColor = col
            newColor = modulate c col
        setColor newColor
        result <- dRender d glPixelRatio tr newColor
        setColor oldColor
        return result
    setColor (Color r g b a) = GL.color $ GL.Color4 r g b a


{-------------------------
  Sprites (bitmap images)
-------------------------}

-- | A Sprite represents a finite bitmap image.
--
-- > [[Sprite]] = [-1,1]^2 -> Color
data Sprite = Sprite { spriteObject :: GL.TextureObject }

-- | Load an image from a file and create a sprite out of it.
openSprite :: FilePath -> IO Sprite
openSprite path = do
    e <- Image.loadImage path
    case e of
        Left err -> fail err
        Right bmp -> Sprite <$> Bitmap.makeSimpleBitmapTexture bmp

-- | The image of a sprite at the origin.
--
-- > [[sprite s]] p | p `elem` [-1,1]^2 = ([[s]] p, Any True)
-- >                | otherwise         = (zero, Any False)
sprite :: Sprite -> Image Any
sprite spr = Image render' pick
    where
    render' _ tr _ = do
        GL.texture GL.Texture2D GL.$= GL.Enabled
        oldtex <- GL.get (GL.textureBinding GL.Texture2D)
        GL.textureBinding GL.Texture2D GL.$= (Just $ spriteObject spr)
        GL.renderPrimitive GL.Quads $ do
            texcoord 0 0
            GL.vertex   $ toVertex tr (-1, 1)
            texcoord 1 0
            GL.vertex   $ toVertex tr (1, 1)
            texcoord 1 1
            GL.vertex   $ toVertex tr (1,-1)
            texcoord 0 1
            GL.vertex   $ toVertex tr (-1,-1)
        GL.textureBinding GL.Texture2D GL.$= oldtex
    pick (x,y) | -1 <= x && x <= 1 && -1 <= y && y <= 1 = Any True
               | otherwise                              = Any False
    texcoord x y = GL.texCoord $ GL.TexCoord2 (x :: GL.GLdouble) (y :: GL.GLdouble)

{---------
 Text
---------}

-- | The image representing some text rendered with a font.  The baseline
-- is at y=0, the text starts at x=0, and the height of a lowercase x is
-- 1 unit.
text :: Font -> String -> Image Any
text font str = Image render' pick
    where
    render' glPixelRatio tr _ = renderText glPixelRatio font str tr
    pick (x,y)
      | 0 <= x && x <= textWidth font str && -0.5 <= y && y <= 1.5 = Any True
      | otherwise                                             = Any False

#ifdef LAME_FONTS

data Font = Font

openFont :: String -> IO Font
openFont _ = do
    inited <- GLUT.get GLUT.initState
    unless inited $ GLUT.initialize "" [] >> return ()
    return Font

renderText :: GLPixelRatio -> Font -> String -> Affine -> IO ()
renderText _ Font str tr = withMultGLmatrix tr $ do
    GL.scale (1/64 :: GL.GLdouble) (1/64) 1
    GLUT.renderString GLUT.Roman str

textWidth :: Font -> String -> R
textWidth Font str = (1/64) * fromIntegral (unsafePerformIO (GLUT.stringWidth GLUT.Roman str))

#else

data Font =
  Font
  { _getFontBySize :: Map Int FTGL.Font
  , getFont72 :: FTGL.Font -- default if more precise size not found
  }

fontSizes :: [Int]
fontSizes = [9,12,15,18,24,36,44]
defaultFontSize :: Int
defaultFontSize = 72

setFontFaceSize :: FTGL.Font -> Int -> IO ()
setFontFaceSize font size =
    do
        1 <- FTGL.setFontFaceSize font size resolution
        return ()
    where
        -- DPI of device. It doesn't seem to affect anything much
        -- (1..72 seem to behave the same). Possibly this is more
        -- useful in non-texture/buffer-fonts:
        resolution :: Int
        resolution = 72

fontForSize :: Int -> Font -> (Int, FTGL.Font)
fontForSize k (Font m def) =
    minimumBy (comparing fst) $
    (72, def) : catMaybes [Map.lookupLE k m, Map.lookupGE k m]

fontHeight :: R
fontHeight = 2

renderText :: GLPixelRatio -> Font -> String -> Affine -> IO ()
renderText (_glXPixels, glYPixels) font str tr =
    withMultGLmatrix tr' $
    FTGL.renderFont sizedFont str FTGL.All
    where
        yScale = scaleYFactor tr
        heightGl = fontHeight
        tr' = tr `compose` scale heightRatio heightRatio
        desiredHeightPixels = heightGl * yScale * glYPixels
        (heightPixels, sizedFont) = fontForSize (round desiredHeightPixels) font
        heightRatio = heightGl / fromIntegral heightPixels

-- | Load a TTF font from a file.
--
-- WARNING: This is unsafe due to the finalizer possibly running earlier than
-- expected
-- See discussion at:
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Mem-Weak.html#v:addFinalizer
openFont :: FilePath -> IO Font
openFont path =
    Font
    <$> (Map.fromList <$> mapM createFont fontSizes)
    <*> (snd <$> createFont defaultFontSize)
    where
        createFont size = do
            font <- FTGL.createBufferFont path
            addFinalizer font (FTGL.destroyFont font)
            _ <- setFontFaceSize font size
            return (size, font)

withFTGLFont :: FilePath -> Int -> (FTGL.Font -> IO a) -> IO a
withFTGLFont path size act =
    Exception.bracket (FTGL.createBufferFont path) FTGL.destroyFont $
    \font -> do
        setFontFaceSize font size
        act font

withFont :: FilePath -> (Font -> IO a) -> IO a
withFont path action = go Map.empty fontSizes
    where
        go fontMap [] =
            withFTGLFont path defaultFontSize $ \defaultFont ->
            action (Font fontMap defaultFont)
        go fontMap (size:sizes) =
            withFTGLFont path size $ \font ->
            go (Map.insert size font fontMap) sizes

-- | @textWidth font str@ is the width of the text in @text font str@.
textWidth :: Font -> String -> R
textWidth font str =
  (* (fontHeight/72)) . realToFrac . unsafePerformIO $
  FTGL.getFontAdvance (getFont72 font) str

#endif

-- | Import an OpenGL action and pure sampler function into an Image.
-- This ought to be a well-behaved, compositional action (make sure
-- it responds to different initial ModelViews, don't change matrix
-- modes or render or anything like that).  The color given to the
-- action is the current tint color; modulate all your colors by this
-- before setting them.
unsafeOpenGLImage :: (Color -> IO ()) -> (R2 -> a) -> Image a
unsafeOpenGLImage draw pick = Image render' pick
    where
    render' _ tr col = GL.preservingAttrib [GL.AllServerAttributes] . withMultGLmatrix tr $ draw col
