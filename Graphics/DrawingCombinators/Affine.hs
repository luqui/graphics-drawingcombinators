{- | An affine transformation is a linear transformation followed by
a translation; i.e. it is a function

> \x -> A*x + b

Where A is a linear transformation.  Affine transformations are the
set of image transformations supported by Graphics.DrawingCombinators,
roughly translate, rotate, scale, and compositions thereof.
-}

module Graphics.DrawingCombinators.Affine
    ( R, R2, Affine
    , compose, apply, identity, translate, rotate, scale, inverse
    , multGLmatrix
    )
where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Monoid

type R = GL.GLdouble
type R2 = (R,R)

-- | An Affine transformation from R2 to R2.  
--
-- > [[Affine]] = R2 -> R2
--
-- With the Monoid instance @(identity, compose)@
data Affine = M !R !R !R
                !R !R !R
             --  0  0  1

instance Monoid Affine where
    mempty = identity
    mappend = compose

-- | > [[compose a b]] = [[a]] . [[b]]
compose :: Affine -> Affine -> Affine
M x11 x12 x13 x21 x22 x23 `compose` M y11 y12 y13 y21 y22 y23 =
    M (x11*y11+x12*y21) (x11*y12+x12*y22) (x11*y13+x12*y23+x13)
      (x21*y11+x22*y21) (x21*y12+x22*y22) (x21*y13+x22*y23+x23)

-- | > [[apply a]] = [[a]]
apply :: Affine -> R2 -> R2
apply (M x11 x12 x13 x21 x22 x23) (y1,y2) = 
    (x11*y1+x12*y2+x13, x21*y1+x22*y2+x23)

-- | > [[identity]] = id
identity :: Affine
identity = M 1 0 0
             0 1 0

-- | > [[inverse x]] = inverse [[x]]
--
-- If the transformation is not invertible, this operation is
-- undefined.
inverse :: Affine -> Affine
inverse (M x11 x12 x13 x21 x22 x23) = 
    M (s*x22)   (-s*x12)  (-s*x22*x13 + s*x12*x23)
      (-s*x21)  (s*x11)   ( s*x21*x13 - s*x11*x23)
    where
    s = 1 / (x11*x22 - x12*x21)

-- | > [[translate t]] x = [[t]] x + t
translate :: R2 -> Affine
translate (x,y) = M 1 0 x
                    0 1 y

-- | > [[rotate r]] (x,y) = (cos(r)x - sin(r)y, sin(r)x + cos(r)y)
rotate :: R -> Affine
rotate t = M cost (-sint) 0
             sint cost    0
    where
    cost = cos t
    sint = sin t

-- | > [[scale xs ys]] (x,y) = (xs*x, ys*y)
scale :: R -> R -> Affine
scale x y = M x 0 0
              0 y 0

-- | Multiply this Affine by the top of the OpenGL matrix stack.
-- Don't mind this, it\'s an implementation detail.
multGLmatrix :: Affine -> IO ()
multGLmatrix (M x11 x12 x13 x21 x22 x23) = do
    m <- GL.newMatrix GL.ColumnMajor [ x11 , x21 , 0 , 0
                                     , x12 , x22 , 0 , 0
                                     , 0   , 0   , 1 , 0
                                     , x13 , x23 , 0 , 1 ]
    GL.multMatrix (m :: GL.GLmatrix R)
