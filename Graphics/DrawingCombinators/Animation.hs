{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.DrawingCombinators.Animation where

import Prelude as P
import Debug.Trace
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Monad.Trans.State
import Graphics.DrawingCombinators hiding (translate, rotate, scale)
import qualified Graphics.DrawingCombinators as Draw

-- affine animations

newtype AffineAnimation = AffineAnimation (R -> Affine)

instance Monoid AffineAnimation where
    mempty = AffineAnimation (const mempty)
    (AffineAnimation f1) `mappend` (AffineAnimation f2) =
        AffineAnimation (\p -> f1 p `mappend` f2 p)

translate :: R2 -> AffineAnimation
translate r =
    AffineAnimation $ (\p -> Draw.translate (r `scaleR2` p))

rotate :: R -> AffineAnimation
rotate r =
    AffineAnimation $ (\p -> Draw.rotate (r * p))

scale :: R2 -> AffineAnimation
scale (x, y) =
    AffineAnimation $ (\p -> Draw.scale (x * p) (y * p))

-- animations

type Action a = R             -- ^ Progress between [0, -1]
             -> Image a
             -> Image a

data Animation a = Animation
  { aniAction   :: Action a
  , aniDuration :: POSIXTime
  }

empty :: Animation a
empty = Animation (const id) 0

runAnimation :: POSIXTime -> Animation a -> Image a -> Image a
runAnimation elapsed (Animation action duration) img =
    let p = progress elapsed duration
    in  action p img

mapAction :: (Action a -> Action a) -> Animation a -> Animation a
mapAction f (Animation action duration) = Animation (f action) duration

-- | convert affine animation to animation.
animate :: AffineAnimation -> POSIXTime -> Animation a
animate (AffineAnimation aff) = Animation (\p img -> aff p %% img)

reverse :: Animation a -> Animation a
reverse = mapAction (\action p img -> action (1.0-p) img)

parallel :: [Animation a] -> Animation a
parallel as =
    Animation action duration
  where
    action p = foldr (.) id $
                 map (\anim -> transProgress duration anim p) as

    duration = maximum (map aniDuration as)

-- TODO. need state
sequence :: [Animation a] -> Animation a
sequence as =
    Animation action duration
  where
    duration = sum (map aniDuration as)
    action p =
        foldr (.) id
           (  map (($ 1.0) . aniAction) completed
           ++ maybeToList mAction
           )
      where
        elapsed = realToFrac p * duration
        (completed, rest) = splitCompleted as elapsed
        --mAction :: Maybe (Image a -> Image a)
        mAction = do
            cur <- listToMaybe rest
            let curElapsed = elapsed - (sum (map aniDuration completed))
                curProgress = progress curElapsed (aniDuration cur)
            return (aniAction cur curProgress)

-- utils

progress :: POSIXTime -> POSIXTime -> R
progress elapsed duration
  | duration <= 0 = 1.0
  | otherwise     = min 1.0 (realToFrac (elapsed/duration))

-- get real progress
transProgress :: POSIXTime -> Animation a -> R -> Image a -> Image a
transProgress duration (Animation ac d) p = ac (progress (realToFrac p * duration) d)

splitCompleted :: [Animation a] -> POSIXTime -> ([Animation a], [Animation a])
splitCompleted as t = loop t as []
  where
    loop _ [] completed = (P.reverse completed, [])
    loop t rest@(a:as) completed
      | t < aniDuration a = (P.reverse completed, rest)
      | otherwise         = loop (t-aniDuration a) as (a:completed)


scaleR2 :: R2 -> R -> R2
scaleR2 (x, y) scale = (x*scale, y*scale)

substractR2 :: R2 -> R2 -> R2
substractR2 (x1, y1) (x2, y2) = (x1-x2, y1-y2)

