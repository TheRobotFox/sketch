module Main (main) where

import Canvas
import Data.ByteString qualified as B
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Vector.Storable qualified as V
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL qualified as GL
import Linear
import Model
import Render
import Stroke
import Svg qualified
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Window (withWindow)

-- | Pen samples like handwriting: a wandering line whose width drifts.
newtype Scribble = Scribble (NonEmpty Point)
  deriving (Show)

instance Arbitrary Scribble where
  arbitrary = do
    n <- chooseInt (1, 60)
    steps <- vectorOf n ((,,) <$> choose (-0.6, 0.6) <*> choose (0.5, 6) <*> choose (0.3, 4))
    let go (p, h, _) (turn, len, r) = let h' = h + turn in (p + V2 (cos h') (sin h') ^* len, h', r)
    pure (Scribble (fmap (\(V2 x y, _, r) -> V3 x y r) (NE.scanl go (V2 100 100, 0, 1) steps)))

-- | A view and strokes as sketch makes them: drawn at some zoom, from a
-- corner on the canvas rounded to a power of ten, numbers in thousandths.
data Drawn = Drawn View [Ink Stroke]
  deriving (Show)

instance Arbitrary Drawn where
  arbitrary = do
    zoom <- (10 **) <$> choose (-6, 12)
    pan <- V2 <$> (toRational <$> (arbitrary :: Gen Double)) <*> (toRational <$> (arbitrary :: Gen Double))
    inks <- listOf do
      Scribble ss <- arbitrary
      z <- (10 **) <$> choose (-6, 12)
      corner <- V2 <$> arbitrary <*> arbitrary
      color <- Color <$> arbitrary <*> arbitrary <*> arbitrary
      locked <- arbitrary
      let v = View (fmap ((/ 1000) . fromInteger) corner) z
      pure [ink {locked} | Just ink <- [finish 1.5 (start v color) {stroke = reverse (toList ss)}]]
    pure (Drawn (View pan zoom) (concat inks))

-- | How far a point lies outside a stroke's ink, from the curve finely cut.
outside :: Stroke -> V2 Double -> Double
outside s p = minimum [capsule (xy a) (xy b) ra rb p | (a@(V3 _ _ ra), b@(V3 _ _ rb)) <- fine s]

-- | A stroke cut into 1024 straight pieces a curve.
fine :: Stroke -> [(Point, Point)]
fine (Stroke cs) = [(a, b) | c <- toList cs, let ps = [at (fromIntegral i / 1024) c | i <- [0 .. 1024 :: Int]], (a, b) <- zip ps (drop 1 ps)]

xy :: Point -> V2 Double
xy (V3 x y _) = V2 x y

testSettings :: Settings
testSettings = Settings {width = 6, smoothing = 1.5, palette = [Color 0 0 0, Color 0x1f 0x4f 0xd1]}

main :: IO ()
main = do
  gpu <- withWindow "test" \_ -> traverse inkedLikeCPU [1, 1e3, 1e6, 1e9, 1e12]
  defaultMain $
    testGroup
      "sketch"
      [ testProperty "a fitted stroke passes within twice the smoothing of every sample" \(Scribble ss) ->
          let s = fit 1.5 ss
           in all (\p -> minimum [segmentDistance a b p | (a, b) <- fine s] <= 3) ss
      , testProperty "halves of a curve trace it" \(Scribble ss) t ->
          let Stroke (c :| _) = fit 1.5 ss
              t' = abs t - fromInteger (floor (abs t))
           in distance (at (t' / 2) c) (at t' (fst (split 0.5 c))) < 1e-9
      , testProperty "zooming keeps the canvas point under the cursor exactly" \(Positive z) x y f ->
          let v = View (V2 3 4) z
              s = V2 x y
           in toCanvas (zoomAt s (1.1 ** f) v) s == toCanvas v s || zoomAt s (1.1 ** f) v == v
      , testProperty "however long the view is zoomed and dragged, its position stays short" $
          forAll (vectorOf 500 arbitrary) \steps ->
          let move v ((sx, sy), f, (dx, dy)) = drag (V2 dx dy) (zoomAt (V2 sx sy) (1.1 ** fromIntegral (f `mod` 7 - 3 :: Int)) v)
              View (V2 x y) _ = foldl move (View 0 1) (steps :: [((Double, Double), Int, (Double, Double))])
              bits = length . takeWhile (> 0) . iterate (`div` 2)
           in all (\q -> bits (numerator q) + bits (denominator q) < 3000) [x, y]
      , testProperty "in a window, the pieces ink what the stroke does" \(Scribble ss) cx cy ->
          forAll (choose (0.1, 100)) \k -> forAll (vectorOf 50 ((,) <$> choose (0, 2 * pi) <*> choose (0, 1))) \spots ->
            let s = fit 1.5 ss
                centre = V2 100 100 + V2 cx cy ^* 10
                radius = 50 / k
                t = 1 / 16 / k
                ps = pieces t centre radius s
                drawn p = minimum (1 / 0 : [capsule (xy a) (xy b) ra rb p | (a@(V3 _ _ ra), b@(V3 _ _ rb)) <- ps])
             in and
                  [ (d > -3 * t || drawn p < 0) && (d < 3 * t || drawn p > 0)
                  | (angle', f) <- spots, let p = centre + V2 (cos angle') (sin angle') ^* (radius * sqrt f), let d = outside s p
                  ]
      , testCase "however deep, a stroke's edge takes a bounded number of pieces" do
          let s = fit 1.5 (V3 0 0 3 :| [V3 40 30 3, V3 80 0 3])
          sequence_
            [ assertBool (show (k, w, n)) (n < 400)
            | k <- [1, 1e3, 1e6, 1e9, 1e12, 1e15, 1e18], w <- [V2 40 (27 - 1e-3), V2 40 (33 + 1e-9)]
            , let n = length (pieces (1 / 16 / k) w (1000 / k) s)
            ]
      , testCase "drawing with the mouse adds a stroke, and a drag drops one being drawn" do
          let run = foldl (flip update) (initial testSettings)
              stroke = [MouseDown (V2 10 10), MouseMove (V2 20 15), MouseMove (V2 30 10), MouseUp]
          length (run stroke).strokes @?= 1
          length (run (take 3 stroke <> [Drag (V2 5 5), MouseUp])).strokes @?= 0
          (run [Key '2', Key '+']).color @?= 2
      -- The scribbles' ink is under twice as wide as this eraser, which cuts it all.
      , testProperty "what the eraser leaves stays clear of it" \(Scribble ss) (NonEmpty path) ->
          let s = fit 1.5 ss
              eraser = fmap (\(x, y) -> V2 100 100 + V2 x y ^* 3) (NE.fromList (take 20 path))
           in all (\(Stroke cs) -> and [norm (xy p - e) - r >= 4 - 0.25 | c <- toList cs, i <- [0 .. 32 :: Int], let p@(V3 _ _ r) = at (fromIntegral i / 32) c, e <- toList eraser]) (fromMaybe [s] (erase 4 1 eraser s))
      , testCase "the eraser cuts a line in two, and leaves template lines" do
          let line = Stroke (V4 (V3 0 0 1) (V3 30 0 1) (V3 70 0 1) (V3 100 0 1) :| [])
          fmap (map (\(Stroke cs) -> let V4 (V3 a _ _) _ _ _ = NE.head cs; V4 _ _ _ (V3 b _ _) = NE.last cs in (round a, round b))) (erase 8 1 (NE.fromList [V2 50 y | y <- [-20, -16 .. 20]]) line) @?= Just [(0 :: Int, 41 :: Int), (59, 100)]
          wipe 8 (View 0 1) (V2 50 (-20) :| [V2 50 20]) (Ink 0 1 (Color 0 0 0) True line) @?= Nothing
      , testCase "undo and redo strokes and erasing, a gesture at a time" do
          let run = foldl (flip update) (initial testSettings)
              stroke y = [MouseDown (V2 10 y), MouseMove (V2 200 y), MouseUp]
              two = stroke 50 <> stroke 100
              count es = length (run es).strokes
          map count [two, two <> [Ctrl 'z'], two <> [Ctrl 'z', Ctrl 'y']] @?= [2, 1, 2]
          let rubbed = two <> [Key '0', MouseDown (V2 100 20), MouseMove (V2 100 80), MouseMove (V2 100 130), MouseUp]
          map count [rubbed, rubbed <> [Ctrl 'z']] @?= [4, 2]
      , testProperty "a drawing saved as SVG reads back exactly" \(Drawn v inks) ->
          case Svg.parse (B8.unpack (B.toStrict (toLazyByteString (Svg.document v inks)))) of
            Just d -> d.view == Just v && d.strokes == inks && null d.skipped
            Nothing -> False
      , testCase "a template reads as locked lines, and what can't be read is named" do
          d <- maybe (assertFailure "not SVG") pure (Svg.parse "<svg><g stroke=\"#a8bfe0\" stroke-width=\"1\" fill=\"none\" data-locked=\"true\"><path d=\"M 40 90 H 600 m 0 30 h -560\"/></g><rect/></svg>")
          map (.locked) d.strokes @?= [True, True]
          [(a, b) | Ink {stroke = Stroke (V4 a _ _ b :| [])} <- d.strokes] @?= [(V3 40 90 0.5, V3 600 90 0.5), (V3 600 120 0.5, V3 40 120 0.5)]
          d.skipped @?= ["rect"]
      , testGroup "the GPU inks what the CPU works out" [testCase ("zoom " <> show z) (assertBool msg ok) | (z, ok, msg) <- gpu]
      ]

-- | Render a few strokes through the GPU, zoomed in about a point on an
-- edge, and compare each pixel with the coverage the CPU works out from the
-- same pieces.
inkedLikeCPU :: Double -> IO (Double, Bool, String)
inkedLikeCPU z = do
  r <- newRenderer
  let strokes = [fit 1.5 (V3 100 100 0.5 :| [V3 200 150 2, V3 300 100 4]), fit 1.5 (V3 150 200 3 :| [V3 160 120 0.2])]
      -- The bottom of the second stroke's round start.
      edge = V2 150 203 - V2 320 240 ^/ z
      m = (initial testSettings) {strokes = Seq.fromList [Ink 0 1 (Color 0 0 0) False s | s <- strokes], view = View (fmap toRational edge) z}
  render r (V2 640 480) m
  GL.bindFramebuffer GL.ReadFramebuffer $= GL.defaultFramebufferObject
  bytes <- V.fromList <$> allocaArray (640 * 480 * 4) \p -> do
    GL.readPixels (GL.Position 0 0) (GL.Size 640 480) (GL.PixelData GL.RGBA GL.UnsignedByte (castPtr p))
    peekArray (640 * 480 * 4) (p :: Ptr GL.GLubyte)
  let shownStrokes = map (shown (V2 640 480) m.view) (toList m.strokes)
      -- Black ink over white, stroke after stroke.
      expected x y = foldl (\v ps -> v * (1 - coverage ps (V2 (x + 0.5) (y + 0.5)))) 255 shownStrokes
      coverage ps q = max 0 (min 1 (0.5 - minimum (1 / 0 : [capsule a b ra rb q | ((a, ra), (b, rb)) <- ps])))
      (worst, at') = maximum [(abs (fromIntegral (bytes V.! (4 * ((479 - y) * 640 + x))) - expected (fromIntegral x) (fromIntegral y)), (x, y)) | y <- [0 .. 479], x <- [0 .. 639]]
      inked = length [() | i <- [0, 4 .. V.length bytes - 1], bytes V.! i < 128]
  pure (z, worst <= 2 && inked > 1000 && inked < 300000, "largest difference " <> show worst <> "/255 at " <> show at' <> ", " <> show inked <> " px inked, " <> show (length (concat shownStrokes)) <> " pieces")
