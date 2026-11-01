-- | Strokes as cubic Bézier curves through (x, y, radius): fitted to pen
-- samples, and cut into straight pieces to draw.
module Stroke
  ( Point
  , Cubic
  , Stroke (..)
  , Box
  , fit
  , split
  , at
  , bounds
  , pieces
  , capsule
  , erase
  , segmentDistance
  ) where

import Data.Foldable (toList)
import Data.List (intercalate, zipWith4)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, isNothing)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Linear

-- | A position and the ink's radius there.
type Point = V3 Double

-- | Start, two control points, end.
type Cubic = V4 Point

-- | Curves joined end to start. A tap is one curve with all its points equal.
newtype Stroke = Stroke (NonEmpty Cubic)
  deriving (Eq, Show)

-- | Lowest and highest corner.
type Box = (V2 Double, V2 Double)

-- | A smooth stroke through pen samples, oldest first, straying from them
-- by about the tolerance at most.
fit :: Double -> NonEmpty Point -> Stroke
fit tolerance = Stroke . catmullRom tolerance . simplify tolerance

-- | Ramer–Douglas–Peucker: keep the ends, and the point between furthest from
-- the line joining them while it's further than the tolerance.
simplify :: Double -> NonEmpty Point -> NonEmpty Point
simplify tolerance (first :| rest) = first :| keep first rest
  where
    keep _ [] = []
    keep start points = case splitAt furthest points of
      (before@(_ : _), after@(_ : _)) | worst > tolerance -> keep start before <> keep (last before) after
      _ -> [end]
      where
        end = last points
        (worst, furthest) = maximum ((0, 0) : zip (map (segmentDistance start end) (init points)) [1 ..])

-- | A Catmull-Rom spline through points, one curve from each to the next.
-- Its handles lie a third of the chord along the direction from the point
-- before to the point after, but stray from the chord by the tolerance at
-- most, so that at a corner it turns tightly instead of overshooting.
catmullRom :: Double -> NonEmpty Point -> NonEmpty Cubic
catmullRom tolerance points@(first :| _) =
  case zipWith4 curve padded (drop 1 padded) (drop 2 padded) (drop 3 padded) of
    c : cs -> c :| cs
    [] -> V4 first first first first :| []
  where
    padded = first : toList points <> [NE.last points]
    curve a b c d = V4 b (b + handle (c - a) (c - b)) (c - handle (d - b) (c - b)) c
    handle direction chord = u ^* min (norm chord / 3) (if away > 0 then tolerance / away else 1 / 0)
      where
        u = if norm direction == 0 then direction else direction ^/ norm direction
        cosine = if norm chord == 0 then 1 else dot u chord / norm chord
        -- How far the handle strays from the chord per unit of its length.
        away = if cosine < 0 then 1 else sqrt (max 0 (1 - cosine * cosine))

-- | Split a curve at a parameter from 0 to 1 (de Casteljau).
split :: Double -> Cubic -> (Cubic, Cubic)
split t (V4 a b c d) = (V4 a ab abc abcd, V4 abcd bcd cd d)
  where
    -- Equal numbers stay equal, so a width that doesn't vary still doesn't.
    mix = liftA2 \p q -> if p == q then p else (1 - t) * p + t * q
    (ab, bc, cd) = (mix a b, mix b c, mix c d)
    (abc, bcd) = (mix ab bc, mix bc cd)
    abcd = mix abc bcd

at :: Double -> Cubic -> Point
at t = (\(V4 _ _ _ p) -> p) . fst . split t

-- | A box around all of a stroke's ink, which keeps within its control
-- points grown by their widest radius.
bounds :: Stroke -> Box
bounds (Stroke curves) = (foldr1 (liftA2 min) xy - pure r, foldr1 (liftA2 max) xy + pure r)
  where
    points = curves >>= \(V4 a b c d) -> a :| [b, c, d]
    xy = fmap (\(V3 x y _) -> V2 x y) points
    r = maximum (fmap (\(V3 _ _ w) -> w) points)

-- | The straight pieces of a stroke whose ink may show in a window, a disc
-- with this centre and radius: each curve halved until it is flat to the
-- tolerance, its ink can't reach the window, or covers it whole. A flat
-- piece off the window is cheaper to draw than to rule out. A curve's
-- (x, y, r) strays from its chord's by no more than its control points do,
-- so its ink stays within twice that of the chord's capsule. Its numbers are
-- only exact to about 2^-52 of the largest, so it is flat enough at 2^-50
-- of that, however small the tolerance.
pieces :: Double -> V2 Double -> Double -> Stroke -> [(Point, Point)]
pieces tolerance centre radius (Stroke curves) = foldMap (\c -> go (max tolerance (2 ^^ (-50 :: Int) * largest c)) (60 :: Int) c) curves
  where
    go t n c@(V4 a@(V3 ax ay ra) b c' d@(V3 dx dy rd))
      | bend <= t || n == 0 = [(a, d)]
      | gap > radius + 2 * bend = []
      | gap + radius < -2 * bend = [(a, d)]
      | otherwise = let (l, r) = split 0.5 c in go t (n - 1) l <> go t (n - 1) r
      where
        bend = max (segmentDistance a d b) (segmentDistance a d c')
        gap = capsule (V2 ax ay) (V2 dx dy) ra rd centre
    largest = maximum . (1 :) . map abs . concatMap toList . toList

-- | What's left of a stroke where an eraser, discs of this radius at these
-- points, wipes it, if it touches: each curve is cut where its ink comes
-- within the eraser, to about a quarter of this pixel, and split exactly
-- there. Ink over twice as wide as the eraser stays, and so do curves it
-- doesn't touch, just as they were; leftovers no longer than it go.
erase :: Double -> Double -> NonEmpty (V2 Double) -> Stroke -> Maybe [Stroke]
erase reach pixel eraser (Stroke curves)
  | all null cuts = Nothing
  | otherwise = Just [Stroke run | Just run <- map (NE.nonEmpty . catMaybes) (splitWhen isNothing kept), sum (fmap legs run) > reach]
  where
    cuts = map (merge . go 0 1) (toList curves)
    kept = concat (zipWith keep cuts (toList curves))
    -- A curve's pieces between its cuts, and Nothing for each cut.
    keep cs c = intercalate [Nothing] [[Just (part a b c) | a < b] | (a, b) <- zip (0 : map snd cs) (map fst cs <> [1])]
    part a b c = snd (split (a / b) (fst (split b c)))
    -- The parameter ranges of a curve whose ink the eraser touches.
    go t0 t1 c@(V4 a@(V3 ax ay ra) b c' d@(V3 dx dy rd))
      | minimum (map (\(V3 _ _ r) -> r) [a, b, c', d]) > 2 * reach = []
      | minimum (fmap (capsule (V2 ax ay) (V2 dx dy) ra rd) eraser) > reach + 2 * bend = []
      | legs c <= pixel / 4 = [(t0, t1) | maximum (map (\(V3 _ _ r) -> r) [a, b, c', d]) <= 2 * reach]
      | otherwise = let (l, r) = split 0.5 c; m = (t0 + t1) / 2 in go t0 m l <> go m t1 r
      where
        bend = max (segmentDistance a d b) (segmentDistance a d c')
    merge ((a, b) : (b', c) : rest) | b == b' = merge ((a, c) : rest)
    merge (r : rest) = r : merge rest
    merge [] = []

-- | Never shorter than a curve.
legs :: Cubic -> Double
legs (V4 a b c d) = distance a b + distance b c + distance c d

-- | How far a point lies outside the hull of two discs, or inside if
-- negative: all the discs between them too, as their centres and radii go
-- evenly from one to the other (Inigo Quilez's uneven capsule).
capsule :: V2 Double -> V2 Double -> Double -> Double -> V2 Double -> Double
capsule a b ra rb p
  | h <= abs (ra - rb) = if ra >= rb then norm (p - a) - ra else norm (p - b) - rb
  | k < 0 = norm q - ra
  | k > c * h = norm (q - V2 0 h) - rb
  | otherwise = dot q (V2 c s) - ra
  where
    h = norm (b - a)
    u = (b - a) ^/ h
    q = V2 (abs (crossZ u (p - a))) (dot (p - a) u)
    s = (ra - rb) / h
    c = sqrt (1 - s * s)
    k = dot q (V2 (-s) c)

-- | The distance from a point to the segment between two others.
segmentDistance :: (Metric f, Floating a, Ord a) => f a -> f a -> f a -> a
segmentDistance a b p = distance p (a ^+^ t *^ ab)
  where
    ab = b ^-^ a
    t = if quadrance ab == 0 then 0 else max 0 (min 1 (dot (p ^-^ a) ab / quadrance ab))
