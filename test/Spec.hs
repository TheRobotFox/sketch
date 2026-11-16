module Main (main) where

import Canvas
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
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
import System.Timeout (timeout)
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
fine s = [(a, b) | c <- toList s.curves, let ps = [at (fromIntegral i / 1024) c | i <- [0 .. 1024 :: Int]], (a, b) <- zip ps (drop 1 ps)]

xy :: Point -> V2 Double
xy (V3 x y _) = V2 x y

testSettings :: Settings
testSettings = Settings {width = 6, smoothing = 1.5, palette = [Color 0 0 0, Color 0x1f 0x4f 0xd1]}

testModel :: Model
testModel = (initial testSettings) {pressure = 0.5}

svgBytes :: View -> [Ink Stroke] -> B.ByteString
svgBytes view = B.toStrict . toLazyByteString . Svg.document view

readSvg :: String -> IO Svg.Drawing
readSvg = maybe (assertFailure "not SVG") pure . Svg.parse

main :: IO ()
main = do
  (gpu, repaints) <- withWindow "test" \_ -> do
    gpu <- traverse inkedLikeCPU [1, 1e3, 1e6, 1e9, 1e12]
    repaints <- eraserRepaints
    pure (gpu, repaints)
  defaultMain $
    testGroup
      "sketch"
      [ testProperty "a fitted stroke passes within twice the smoothing of every sample" \(Scribble ss) ->
          let s = fit 1.5 ss
           in all (\p -> minimum [segmentDistance a b p | (a, b) <- fine s] <= 3) ss
      , testProperty "halves of a curve trace it" \(Scribble ss) t ->
          let c = NE.head (fit 1.5 ss).curves
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
      , testCase "drawing with the pen adds a stroke, and a drag drops one being drawn" do
          let run = foldl (flip update) (testModel)
              stroke = [PenDown False (V2 10 10), PenMove (V2 20 15), PenMove (V2 30 10), PenUp]
          length (run stroke).strokes @?= 1
          length (run (take 3 stroke <> [Drag (V2 5 5), PenUp])).strokes @?= 0
          (run [Key '2', Key '+']).color @?= 2
      -- The scribbles' ink is under twice as wide as this eraser, which cuts it all.
      , testProperty "what the eraser leaves stays clear of it" \(Scribble ss) (NonEmpty path) ->
          let s = fit 1.5 ss
              eraser = fmap (\(x, y) -> V2 100 100 + V2 x y ^* 3) (NE.fromList (take 20 path))
           in all (\cut -> and [norm (xy p - e) - r >= 4 - 0.25 | c <- toList cut.curves, i <- [0 .. 32 :: Int], let p@(V3 _ _ r) = at (fromIntegral i / 32) c, e <- toList eraser]) (fromMaybe [s] (erase 4 1 eraser s))
      , testCase "the eraser cuts a line in two, and leaves template lines" do
          let line = fromCurves (V4 (V3 0 0 1) (V3 30 0 1) (V3 70 0 1) (V3 100 0 1) :| [])
          fmap (map (\cut -> let V4 (V3 a _ _) _ _ _ = NE.head cut.curves; V4 _ _ _ (V3 b _ _) = NE.last cut.curves in (round a, round b))) (erase 8 1 (NE.fromList [V2 50 y | y <- [-20, -16 .. 20]]) line) @?= Just [(0 :: Int, 41 :: Int), (59, 100)]
          wipe 8 (View 0 1) (V2 50 (-20) :| [V2 50 20]) (Ink 0 1 (Color 0 0 0) True line) @?= Nothing
      , testCase "erasing rebuilds runs across disjoint cuts and curve boundaries" do
          let line a b = V4 (V3 a 0 1) (V3 (a + (b - a) / 3) 0 1) (V3 (b - (b - a) / 3) 0 1) (V3 b 0 1)
              ends cut = let V4 (V3 a _ _) _ _ _ = NE.head cut.curves; V4 _ _ _ (V3 b _ _) = NE.last cut.curves in (round a :: Int, round b :: Int)
          forM_
            [ ("disjoint cuts", [0, 200], V2 50 (-20) :| [V2 50 20, V2 150 20, V2 150 (-20)], [(0, 41), (59, 141), (159, 200)])
            , ("cut at a join", [0, 100, 200], V2 100 (-20) :| [V2 100 20], [(0, 91), (109, 200)])
            , ("whole curves removed", [0, 60, 90, 120, 180], V2 60 0 :| [V2 120 0], [(0, 51), (129, 180)])
            , ("short island removed", [0, 100], V2 40 (-20) :| [V2 40 20, V2 62 20, V2 62 (-20)], [(0, 31), (71, 100)])
            ] \(caseName, points, path, expected) -> do
              let s = fromCurves (NE.fromList (zipWith line points (drop 1 points)))
              assertEqual caseName (Just expected) (map ends <$> erase 8 1 path s)
      , testCase "erasing preserves untouched curves and their exact joins" do
          let join = V3 80.123456789 0 1
              first = V4 (V3 0 0 1) (V3 20 0 1) (V3 60 0 1) join
              lastCurve = V4 join (V3 100 0 1) (V3 160 0 1) (V3 200 0 1)
          case erase 8 1 (V2 30 (-20) :| [V2 30 20]) (fromCurves (first :| [lastCurve])) of
            Just [_, right] -> do
              length right.curves @?= 2
              NE.last right.curves @?= lastCurve
              let V4 _ _ _ end = NE.head right.curves
              end @?= join
            result -> assertFailure (show result)
      , testProperty "a fast eraser sweep leaves no ink between its input events" \(Scribble ss) ->
          let from = V2 70 100
              to = V2 160 100
              leftovers = fromMaybe [fit 1.5 ss] (erase 4 1 (from :| [to]) (fit 1.5 ss))
           in all (\cut -> and [segmentDistance from to (xy p) - r >= 3.75 | c <- toList cut.curves, i <- [0 .. 32 :: Int], let p@(V3 _ _ r) = at (fromIntegral i / 32) c]) leftovers
      , testCase "a fully covered long curve is removed without subdividing its length" do
          let line = fromCurves (V4 (V3 0 0 1) (V3 3e8 0 1) (V3 6e8 0 1) (V3 9e8 0 1) :| [])
          result <- timeout 2000000 (evaluate (erase 1e9 1 (V2 0 0 :| []) line))
          result @?= Just (Just [])
      , testCase "zero pressure erasing changes no ink or undo history" do
          let drawn = foldl (flip update) (testModel) [PenDown False (V2 10 50), PenMove (V2 200 50), PenUp]
              events = [PenPressure 0 (V2 100 0), PenDown True (V2 100 0), PenMove (V2 100 100), PenUp]
          result <- timeout 2000000 (evaluate ((foldl (flip update) drawn events).edits))
          result @?= Just drawn.edits
          let rubbed = foldl (flip update) drawn events
          rubbed.strokes @?= drawn.strokes
          length rubbed.past @?= length drawn.past
      , testCase "erasing preserves fine coordinates when zoomed in" do
          let line = fromCurves (V4 (V3 0 0 1e-6) (V3 3e-5 0 1e-6) (V3 7e-5 0 1e-6) (V3 1e-4 0 1e-6) :| [])
          case erase 8e-6 1e-6 (V2 5e-5 (-2e-5) :| [V2 5e-5 2e-5]) line of
            Just [left, right] -> do
              let V4 _ _ _ (V3 x _ _) = NE.last left.curves
                  V4 (V3 y _ _) _ _ _ = NE.head right.curves
              assertBool (show (x, y)) (x > 4e-5 && x < 4.2e-5 && y > 5.8e-5 && y < 6e-5)
            result -> assertFailure (show result)
      , testCase "undo and redo strokes and erasing, a gesture at a time" do
          let run = foldl (flip update) (testModel)
              stroke y = [PenDown False (V2 10 y), PenMove (V2 200 y), PenUp]
              two = stroke 50 <> stroke 100
              count es = length (run es).strokes
          map count [two, two <> [Ctrl 'z'], two <> [Ctrl 'z', Ctrl 'y']] @?= [2, 1, 2]
          let rubbed = two <> [Key '0', PenDown False (V2 100 20), PenMove (V2 100 80), PenMove (V2 100 130), PenUp]
          map count [rubbed, rubbed <> [Ctrl 'z'], rubbed <> [Ctrl 'z', Ctrl 'y']] @?= [4, 2, 4]
          (run (rubbed <> [Ctrl 'z', Ctrl 'y'])).strokes @?= (run rubbed).strokes
      , testProperty "a drawing saved as SVG reads back exactly" \(Drawn v inks) ->
          case Svg.parse (B8.unpack (svgBytes v inks)) of
            Just d -> d.view == Just v && d.strokes == inks && null d.skipped
            Nothing -> False
      , testProperty "erased strokes saved as SVG read back exactly" \(Scribble ss) ->
          let s = fit 1.5 ss
              inks = [Ink 0 1 (Color 0 0 0) False cut | cut <- fromMaybe [s] (erase 4 1 (V2 100 70 :| [V2 100 150]) s)]
           in case Svg.parse (B8.unpack (svgBytes (View 0 1) inks)) of
                Just d -> d.strokes == inks && null d.skipped
                Nothing -> False
      , testCase "erased coordinates and SVG commands stay compact" do
          let line = fromCurves (V4 (V3 0 0 1) (V3 30 0 1) (V3 70 0 1) (V3 100 0 1) :| [])
              inks = [Ink 0 1 (Color 0 0 0) False s | s <- fromMaybe [line] (erase 8 1 (V2 50 (-20) :| [V2 50 20]) line)]
              saved = svgBytes (View 0 1) inks
          assertBool (show (B.length saved)) (B.length saved < 450)
          d <- readSvg (B8.unpack saved)
          d.strokes @?= inks
      , testCase "old repeated-C and compact native paths load identically" do
          let load d = readSvg ("<svg><path stroke=\"#000\" stroke-width=\"8\" data-r=\"1 2 3 4 5 6 7\" transform=\"translate(10 20) scale(2)\" d=\"" <> d <> "\"/></svg>")
          old <- load "M0 0C1 2 3 4 5 6C7 8 9 10 11 12"
          compact <- load "M0 0C1 2 3 4 5 6 7 8 9 10 11 12"
          map (length . (.stroke.curves)) old.strokes @?= [2]
          old.strokes @?= compact.strokes
          old.skipped @?= []
      , testCase "a native pen tap round trips" do
          let inks = [Ink 0 1 (Color 0x12 0x34 0x56) False (fromCurves (pure (V3 10.125 20.25 0.375) :| []))]
          saved <- readSvg (B8.unpack (svgBytes (View 0 1) inks))
          saved.strokes @?= inks
      , testCase "A4 karo keeps all 103 locked grid lines, placement and SVG round trips" do
          d <- readSvg . B8.unpack =<< B.readFile "a4_karo.svg"
          d.view @?= Just (View (V2 (-50) (-50)) 10)
          d.skipped @?= []
          map (.color) d.strokes @?= replicate 103 (Color 0xdf 0xdf 0xdf)
          map (.locked) d.strokes @?= replicate 103 True
          let ends = [(a, b) | ink <- d.strokes, V4 a _ _ b :| [] <- [ink.stroke.curves]]
          ends @?= [(V3 0 y 0.15, V3 210 y 0.15) | y <- [0, 5 .. 295]] <> [(V3 x 0 0.15, V3 x 297 0.15) | x <- [0, 5 .. 210]]
          map (wipe 30 (View 0 1) (V2 100 100 :| [])) d.strokes @?= replicate 103 Nothing
          saved <- readSvg (B8.unpack (svgBytes (View (V2 (-50) (-50)) 10) d.strokes))
          saved.strokes @?= d.strokes
          saved.view @?= d.view
      , testCase "the bundled lined and panel templates use the supported subset" do
          forM_ [("templates/lined.svg", 28), ("templates/panels.svg", 6)] \(file, count) -> do
            d <- readSvg . B8.unpack =<< B.readFile file
            d.skipped @?= []
            length d.strokes @?= count
            assertBool file (all (.locked) d.strokes)
      , testCase "plain lines inherit color, width and locking without inheriting coordinates" do
          d <- readSvg "<svg stroke=\"#abc\" stroke-width=\"2\"><g data-locked=\"true\" x1=\"100\"><line x2=\"3\"/><line x1=\"4\" y1=\"5\" x2=\"8\" y2=\"9\" stroke=\"#123456\" stroke-width=\"4\"/></g></svg>"
          d.strokes @?= [Ink 0 1 (Color 0xaa 0xbb 0xcc) True (fromCurves (V4 (V3 0 0 1) (V3 0 0 1) (V3 3 0 1) (V3 3 0 1) :| [])), Ink 0 1 (Color 0x12 0x34 0x56) True (fromCurves (V4 (V3 4 5 2) (V3 4 5 2) (V3 8 9 2) (V3 8 9 2) :| []))]
      , testCase "unsupported SVG and malformed data are reported, and following lines still load" do
          forM_
            [ ("<rect width=\"10\" height=\"20\"/>", "rect")
            , ("<path d=\"M0 0L3 0\"/>", "path with an unreadable d")
            , ("<path d=\"m0 0c0 0 3 0 3 0\"/>", "path with an unreadable d")
            , ("<path d=\"M0 0C0 0 3 0 3 0M4 0C4 0 6 0 6 0\"/>", "path with an unreadable d")
            , ("<path d=\"M0 0C0 0 3 0 3\"/>", "path with an unreadable d")
            , ("<path d=\"M0 0C0 0 3 0 3 0\" data-r=\"1 2\"/>", "path with an unreadable data-r")
            , ("<line x1=\"bad\"/>", "line with an unreadable x1")
            , ("<line x2=\"1e999\"/>", "line with an unreadable x2")
            , ("<line stroke-width=\"-1\"/>", "line with an unreadable stroke-width")
            , ("<line transform=\"rotate(30)\"/>", "line with an unreadable transform")
            , ("<line style=\"stroke:#fff\"/>", "line with style")
            , ("<g transform=\"translate(10 20)\"><line x2=\"999\"/></g>", "styled or transformed group")
            , ("<g style=\"stroke:#fff\"><line x2=\"999\"/></g>", "styled or transformed group")
            ] \(shape, why) -> do
              d <- readSvg ("<svg stroke=\"#000\">" <> shape <> "<line x2=\"3\"/></svg>")
              d.skipped @?= [why]
              length d.strokes @?= 1
      , testGroup "the GPU inks what the CPU works out" [testCase ("zoom " <> show z) (assertBool msg ok) | (z, ok, msg) <- gpu]
      , testGroup "erasing repaints match a full redraw" [testCase ("frame " <> show n) (assertBool "pixels differ" ok) | (n, ok) <- zip [0 :: Int ..] repaints]
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
      m = (testModel) {strokes = Seq.fromList [Ink 0 1 (Color 0 0 0) False s | s <- strokes], view = View (fmap toRational edge) z}
  render r (V2 640 480) m
  bytes <- pixels (V2 640 480)
  let shownStrokes = map (shown (0, V2 640 480) m.view) (toList m.strokes)
      -- Black ink over white, stroke after stroke.
      expected x y = foldl (\v ps -> v * (1 - coverage ps (V2 (x + 0.5) (y + 0.5)))) 255 shownStrokes
      coverage ps q = max 0 (min 1 (0.5 - minimum (1 / 0 : [capsule a b ra rb q | ((a, ra), (b, rb)) <- ps])))
      (worst, at') = maximum [(abs (fromIntegral (bytes V.! (4 * ((479 - y) * 640 + x))) - expected (fromIntegral x) (fromIntegral y)), (x, y)) | y <- [0 .. 479], x <- [0 .. 639]]
      inked = length [() | i <- [0, 4 .. V.length bytes - 1], bytes V.! i < 128]
  pure (z, worst <= 2 && inked > 1000 && inked < 300000, "largest difference " <> show worst <> "/255 at " <> show at' <> ", " <> show inked <> " px inked, " <> show (length (concat shownStrokes)) <> " pieces")

pixels :: V2 Int -> IO (V.Vector GL.GLubyte)
pixels (V2 w h) = do
  GL.bindFramebuffer GL.ReadFramebuffer $= GL.defaultFramebufferObject
  V.fromList <$> allocaArray (w * h * 4) \p -> do
    GL.readPixels (GL.Position 0 0) (GL.Size (fromIntegral w) (fromIntegral h)) (GL.PixelData GL.RGBA GL.UnsignedByte (castPtr p))
    peekArray (w * h * 4) (p :: Ptr GL.GLubyte)

-- | Exercise the cached layer during a gesture, then compare each frame
-- with a renderer that redraws the whole document. Include overlapping
-- colours, locked ink, offscreen cuts, undo/redo and a resize mid-gesture.
eraserRepaints :: IO [Bool]
eraserRepaints = do
  let line color locked y = Ink 0 1 color locked (fit 1 (V3 50 y 2 :| [V3 250 y 2]))
      m0 = (testModel) {strokes = Seq.fromList [line (Color 0 0 0) True 99, line (Color 0xff 0 0) False 100, line (Color 0 0 0xff) False 102, line (Color 0 0 0) False 350, line (Color 0 0 0) False (-50)]}
      events = [Key '0', PenDown False (V2 100 70), PenMove (V2 100 120), PenMove (V2 160 120), PenMove (V2 160 70), PenUp, Ctrl 'z', Ctrl 'y', PenDown False (V2 100 (-70)), PenMove (V2 100 (-30)), PenUp]
      models = scanl (flip update) m0 events
      frames = [(size, m) | (n, m) <- zip [0 :: Int ..] models, size <- if n == 4 then [V2 640 480, V2 320 240, V2 640 480] else [V2 640 480]]
  cached <- newRenderer
  actual <- forM frames \(size, m) -> render cached size m >> pixels size
  fresh <- newRenderer
  expected <- forM (zip [1 :: Int ..] frames) \(n, (size, m)) -> render fresh size m {erasing = Nothing, edits = n} >> pixels size
  pure (zipWith (==) actual expected)
