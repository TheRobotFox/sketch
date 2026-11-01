-- | Strokes on an endless canvas, and the view onto it. Canvas positions are
-- exact, so the view can zoom into any spot however far; each stroke keeps
-- its points in the pixels of the view it was drawn in, as Doubles.
module Canvas
  ( Pos
  , Color (..)
  , Ink (..)
  , View (..)
  , Piece
  , toCanvas
  , zoomAt
  , drag
  , start
  , addSample
  , finish
  , shown
  , live
  , wipe
  ) where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Word (Word8)
import Linear
import Stroke (Point, Stroke (..), erase, fit, pieces)

type Pos = V2 Rational

data Color = Color Word8 Word8 Word8
  deriving (Eq, Show)

-- | Something drawn: its point q lies on the canvas at origin + q * scale.
-- Template lines are locked, for the eraser to leave alone.
data Ink a = Ink {origin :: Pos, scale :: Double, color :: Color, locked :: Bool, stroke :: a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | The canvas point at the window's top-left corner, and window pixels per
-- canvas unit.
data View = View {pan :: Pos, zoom :: Double}
  deriving (Eq, Show)

-- | A straight piece of ink in window pixels: two ends and their radii.
type Piece = ((V2 Double, Double), (V2 Double, Double))

toCanvas :: View -> V2 Double -> Pos
toCanvas v s = v.pan + across v.zoom s

-- | A distance in the window, at a zoom, on the canvas: worked out in
-- Doubles, a hair off, but then taken exactly, so that the view's position
-- only ever adds Doubles and its fractions stay as short as theirs.
across :: Double -> V2 Double -> Pos
across zoom s = fmap toRational (s ^/ zoom)

-- | Zoom by a factor about a window position, which keeps its canvas point.
-- Doubles hold zooms from 1e-300 to 1e300.
zoomAt :: V2 Double -> Double -> View -> View
zoomAt s factor v
  | z >= 1e-300 && z <= 1e300 = View {pan = toCanvas v s - across z s, zoom = z}
  | otherwise = v
  where
    z = v.zoom * factor

-- | Move the view with a drag of this many window pixels.
drag :: V2 Double -> View -> View
drag d v = v {pan = v.pan - across v.zoom d}

-- | A stroke about to be drawn from the view, in its window pixels. Its
-- origin is the view's corner rounded down to a power of ten no bigger than
-- a pixel, which keeps its decimals short.
start :: View -> Color -> Ink [Point]
start v color = Ink {origin = fmap snap v.pan, scale = 1 / v.zoom, color, locked = False, stroke = []}
  where
    step = 10 ^^ (floor (logBase 10 (1 / v.zoom)) :: Integer) :: Rational
    snap x = fromInteger (floor (x / step)) * step

-- | Add where the pen is in the window, with its width there: the pen's
-- width times the pressure, from 0 to 1.
addSample :: View -> V2 Double -> Double -> Ink [Point] -> Ink [Point]
addSample v s width ink = (V3 x y (width / 2 / k) :) <$> ink
  where
    (V2 x y, k) = (inDrawing v s ink, ink.scale * v.zoom)

-- | Fit the samples, newest first, into a stroke smoothed to this many
-- pixels, its numbers rounded to thousandths so that they print short.
finish :: Double -> Ink [Point] -> Maybe (Ink Stroke)
finish smoothing = traverse \samples -> round3 . fit smoothing <$> nonEmpty (reverse samples)
  where
    round3 (Stroke cs) = Stroke (fmap (fmap (fmap \x -> fromInteger (round (x * 1000)) / 1000)) cs)

-- | A window position in a drawing's own pixels.
inDrawing :: View -> V2 Double -> Ink a -> V2 Double
inDrawing v s ink = fmap fromRational ((toCanvas v s - ink.origin) ^/ toRational ink.scale)

-- | How a drawing shows in the window: its point q at at + (q - from) * k,
-- measured from its point at this window position, which alone needs exact
-- sums; the rest are differences of nearby numbers.
placement :: V2 Double -> View -> Ink a -> (V2 Double, V2 Double, Double)
placement s v ink = (from, at, k)
  where
    from = inDrawing v s ink
    at = fmap fromRational ((ink.origin + fmap toRational from ^* toRational ink.scale - v.pan) ^* toRational v.zoom)
    k = ink.scale * v.zoom

-- | The pieces of a stroke that show in a window of this size, flat to a
-- 16th of a pixel and at least a pixel wide. The window reaches a pixel
-- further, for that width and the fading edge.
shown :: V2 Double -> View -> Ink Stroke -> [Piece]
shown size v ink = map (onScreen p) (pieces (1 / 16 / k) from ((norm size / 2 + 1) / k) ink.stroke)
  where
    p@(from, _, k) = placement (size / 2) v ink

-- | The stroke being drawn, from sample to sample.
live :: V2 Double -> View -> Ink [Point] -> [Piece]
live size v ink = case ink.stroke of
  [q] -> [onScreen p (q, q)]
  qs -> zipWith (curry (onScreen p)) qs (drop 1 qs)
  where
    p = placement (size / 2) v ink

onScreen :: (V2 Double, V2 Double, Double) -> (Point, Point) -> Piece
onScreen (from, at, k) (V3 ax ay ra, V3 bx by rb) = (end (V2 ax ay) ra, end (V2 bx by) rb)
  where
    end q r = (at + (q - from) ^* k, max 0.5 (r * k))

-- | What's left of a stroke where an eraser of this radius, in window
-- pixels, wipes along a path in the window, if it touches. Template lines
-- stay.
wipe :: Double -> View -> NonEmpty (V2 Double) -> Ink Stroke -> Maybe [Ink Stroke]
wipe radius v path@(s0 :| _) ink
  | ink.locked = Nothing
  | otherwise = map (\s -> ink {stroke = s}) <$> erase (radius / k) (1 / k) (fmap (\s -> from + (s - at) ^/ k) path) ink.stroke
  where
    (from, at, k) = placement s0 v ink
