module Graphics.DrawingCombinators.Affine
    ( R, Vector2, Affine
    , compose, apply, identity, translate, rotate, scale
    )
where

import Graphics.Rendering.OpenGL.GL (GLdouble)
import Data.Monoid

type R = GLdouble
type Vec2 = (R,R)

data Affine = M !R !R !R
                !R !R !R
             --  0  0  1

instance Monoid Affine where
    mempty = identity
    mappend = compose

compose :: Affine -> Affine -> Affine
M x11 x12 x13 x21 x22 x23 `compose` M y11 y12 y13 y21 y22 y23 =
    M (x11*y11+x12*y21) (x11*y12+x12*y22) (x11*y13+x12*y23+x13)
      (x21*y11+x22*y21) (x21*y12+x22*y22) (x21*y13+x22*y23+x23)

apply :: Affine -> Vector2 -> Vector2
apply (M x11 x12 x13 x21 x22 x23) (y1,y2) = 
    (x11*y1+x12*y2+x13, x21*y1+x22*y2+x23)

identity :: Affine
identity = M 1 0 0
             0 1 0

translate :: Vector2 -> Affine
translate (x,y) = M 1 0 x
                    0 1 y

rotate :: R -> Affine
rotate t = M cost (-sint) 0
             sint cost    0
    where
    cost = cos t
    sint = sin t

scale :: R -> R -> Affine
scale x y = M x 0 0
              0 y 0

