{-# LANGUAGE OverloadedStrings #-}

-- | Drawings as SVG that any viewer shows, one path per stroke (see
-- docs/format.md): written exactly, and read back exactly, along with the
-- plain paths, lines and rectangle outlines other programs write.
module Svg (Drawing (..), document, parse) where

import Canvas (Color (..), Ink (..), Pos, View (..))
import Control.Monad (foldM, guard)
import Data.ByteString.Builder
import Data.Char (isLower, isSpace, toUpper)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Void (Void)
import Linear
import Numeric (readHex, showHex)
import Stroke (Stroke (..), bounds)
import Text.HTML.TagSoup (Tag (..), parseTags)
import Text.Megaparsec hiding (Pos, parse)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char

data Drawing = Drawing
  { view :: Maybe View
  , strokes :: [Ink Stroke]
  , skipped :: [String] -- ^ what couldn't be read, such as "rect" or "path with arcs"
  }

-- * Writing

document :: View -> [Ink Stroke] -> Builder
document v inks =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"" <> spaced box <> "\" data-sketch=\"2\" data-view=\""
    <> decimal x <> " " <> decimal y <> " " <> double v.zoom <> "\">\n"
    <> "<g fill=\"none\" stroke-linecap=\"round\" stroke-linejoin=\"round\">\n"
    <> foldMap path inks
    <> "</g>\n</svg>\n"
  where
    V2 x y = v.pan
    -- Where the strokes lie, with a margin, for viewers.
    box = case nonEmpty [(place i lo, place i hi) | i <- inks, let (lo, hi) = bounds i.stroke] of
      Nothing -> [0, 0, 640, 480]
      Just bs ->
        let lo = foldr1 (liftA2 min) (fmap fst bs)
            hi = foldr1 (liftA2 max) (fmap snd bs)
            margin = pure (maximum (hi - lo) / 40)
         in toList (lo - margin) <> toList (hi - lo + 2 * margin)
    place i q = fmap fromRational i.origin + q ^* i.scale

path :: Ink Stroke -> Builder
path i =
  "<path"
    <> (if i.origin == 0 && i.scale == 1 then "" else " transform=\"translate(" <> decimal ox <> " " <> decimal oy <> ") scale(" <> double i.scale <> ")\"")
    <> (if i.locked then " data-locked=\"true\"" else "")
    <> " stroke=\"" <> hex i.color <> "\" stroke-width=\"" <> double (2 * if constant then r0 else sum radii / fromIntegral (length radii)) <> "\""
    <> " d=\"M " <> xy a <> foldMap (\(V4 _ b c d) -> " C " <> xy b <> " " <> xy c <> " " <> xy d) curves <> "\""
    <> (if constant then "" else " data-r=\"" <> spaced radii <> "\"")
    <> "/>\n"
  where
    V2 ox oy = i.origin
    Stroke curves@(V4 a _ _ _ :| _) = i.stroke
    radii = [r | (n, V4 p b c d) <- zip [0 :: Int ..] (toList curves), V3 _ _ r <- [p | n == 0] <> [b, c, d]]
    V3 _ _ r0 = a
    constant = all (== r0) radii
    xy (V3 px py _) = double px <> " " <> double py
    hex (Color r g b) = "#" <> foldMap (\c -> string7 (if c < 16 then '0' : showHex c "" else showHex c "")) [r, g, b]

spaced :: [Double] -> Builder
spaced = mconcat . zipWith (<>) ("" : repeat " ") . map double

-- | The shortest decimal that reads back as the same Double.
double :: Double -> Builder
double d
  | d == fromInteger (round d) && abs d < 1e15 = integerDec (round d)
  | abs d >= 1e-4 && abs d < 1e15 = formatDouble standardDefaultPrecision d
  | otherwise = formatDouble scientific d

-- | A rational with a finite decimal expansion, as all positions here
-- have, written exactly.
decimal :: Rational -> Builder
decimal q
  | q < 0 = "-" <> decimal (negate q)
  | otherwise = integerDec whole <> (if places == 0 then "" else "." <> string7 (pad (show part)))
  where
    places = length (takeWhile (\n -> denominator (q * 10 ^ n) /= 1) [0 :: Int ..])
    (whole, part) = numerator (q * 10 ^ places) `divMod` (10 ^ places)
    pad s = replicate (places - length s) '0' <> s

-- * Reading

type Parser = Parsec Void String
type Attributes = [(String, String)]

-- | What a file holds, if it's SVG at all.
parse :: String -> Maybe Drawing
parse text = case dropWhile (not . isOpen "svg") (parseTags text) of
  TagOpen _ root : rest ->
    let (inks, skipped) = walk [inherited (styled root)] [] rest
        view = do
          [x, y, z] <- traverse (P.parseMaybe number) . words =<< lookup "data-view" root
          guard (z >= 1e-300 && z <= 1e300)
          pure (View (V2 x y) (fromRational z))
     in Just Drawing {view, strokes = inks, skipped}
  _ -> Nothing
  where
    isOpen name = \case TagOpen n _ -> n == name; _ -> False

-- | The strokes of the paths in these tags, and what was skipped, with the
-- attributes of the groups they are in, innermost first.
walk :: [Attributes] -> [String] -> [Tag String] -> ([Ink Stroke], [String])
walk groups skipped = \case
  [] -> ([], skipped)
  TagOpen "g" as : rest
    | Just _ <- lookup "transform" as -> walk groups ("transformed group" : skipped) (after "g" rest)
    | otherwise -> walk (inherited (styled as) : groups) skipped rest
  TagClose "g" : rest -> walk (drop 1 groups) skipped rest
  TagOpen name as : rest
    | name `elem` ["path", "line", "rect"] -> case readShape name (styled as <> concat groups) of
        Right ink -> let (inks, s) = walk groups skipped rest in (ink <> inks, s)
        Left why -> walk groups (why : skipped) rest
    | name `elem` ["title", "desc", "metadata"] -> walk groups skipped (after name rest)
    | otherwise -> walk groups (name : skipped) (after name rest)
  _ : rest -> walk groups skipped rest

-- | Presentation properties and sketch's lock flag inherit; geometry does not.
inherited :: Attributes -> Attributes
inherited = filter (\(key, _) -> key `elem` ["stroke", "stroke-width", "data-locked"])

-- | Inline style wins over presentation attributes; the last declaration wins.
styled :: Attributes -> Attributes
styled as = reverse [(trim k, trim v) | d <- splitOn ";" (fromMaybe "" (lookup "style" as)), (k, ':' : v) <- [break (== ':') d]] <> as
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | The tags after an element's end.
after :: String -> [Tag String] -> [Tag String]
after name = go (1 :: Int)
  where
    go 0 ts = ts
    go n (TagOpen m _ : ts) | m == name = go (n + 1) ts
    go n (TagClose m : ts) | m == name = go (n - 1) ts
    go n (_ : ts) = go n ts
    go _ [] = []

readShape :: String -> Attributes -> Either String [Ink Stroke]
readShape name as = case lookup "stroke" as of
  Nothing -> Right []
  Just "none" -> Right []
  Just stroke -> do
    width <- readLength name as "stroke-width" 1
    guardShape (width >= 0) (name <> " with a negative stroke-width")
    (origin, scale) <- maybe (Right (0, 1)) (maybe (Left (name <> " with a transform other than translate and scale")) Right . P.parseMaybe transform) (lookup "transform" as)
    parts <- readGeometry name as
    let radii = readRadii width parts (if name == "path" then lookup "data-r" as else Nothing)
        ink = Ink {origin, scale, color = fromMaybe (Color 0 0 0) (readColor stroke), locked = lookup "data-locked" as == Just "true", stroke = ()}
    Right [ink {stroke = Stroke curves} | width > 0, part <- parts, Just curves <- [nonEmpty (cubics radii part)]]

-- | Stored pressure radii follow a stroke-width another program has changed.
readRadii :: Double -> [Subpath] -> Maybe String -> [Double]
readRadii width parts value = case (mapM (P.parseMaybe number) . words =<< value, parts) of
  (Just rs@(_ : _), [(_, cs)]) | length rs == 1 + 3 * length cs ->
    let ds = map fromRational rs
        mean = sum ds / fromIntegral (length ds)
     in if mean > 0 && abs (width - 2 * mean) > 1e-9 * width then map (* (width / 2 / mean)) ds else ds
  _ -> repeat (width / 2)

-- | Attach radii to geometry. A move on its own is a tap.
cubics :: [Double] -> Subpath -> [V4 (V3 Double)]
cubics (r : _) (start, []) = [V4 p p p p | let p = at3 start r]
cubics rs (start, cs) = zipWith3 curve (start : [d | (_, _, d) <- cs]) cs (quads rs)
  where
    curve a (b, c, d) (ra, rb, rc, rd) = V4 (at3 a ra) (at3 b rb) (at3 c rc) (at3 d rd)
    quads (ra : rb : rc : rest@(rd : _)) = (ra, rb, rc, rd) : quads rest
    quads _ = []

at3 :: V2 Double -> Double -> V3 Double
at3 (V2 x y) = V3 x y

readColor :: String -> Maybe Color
readColor ('#' : h) | [(v, "")] <- readHex h = case length h of
  3 -> Just (Color (17 * fromInteger (v `div` 256)) (17 * fromInteger (v `div` 16 `mod` 16)) (17 * fromInteger (v `mod` 16)))
  6 -> Just (Color (fromInteger (v `div` 65536)) (fromInteger (v `div` 256 `mod` 256)) (fromInteger (v `mod` 256)))
  _ -> Nothing
readColor _ = Nothing

type Subpath = (V2 Double, [(V2 Double, V2 Double, V2 Double)])

-- | Shapes become the same cubic geometry as paths, before styling.
readGeometry :: String -> Attributes -> Either String [Subpath]
readGeometry name as = case name of
  "line" -> do
    a <- V2 <$> len "x1" 0 <*> len "y1" 0
    b <- V2 <$> len "x2" 0 <*> len "y2" 0
    pure [(a, [straight a b])]
  "rect" -> do
    x <- len "x" 0
    y <- len "y" 0
    w <- len "width" 0
    h <- len "height" 0
    rx <- len "rx" 0
    ry <- len "ry" 0
    guardShape (all (>= 0) [w, h, rx, ry]) "rect with negative dimensions or corner radii"
    let rx' = min (w / 2) (if lookup "rx" as == Nothing then ry else rx)
        ry' = min (h / 2) (if lookup "ry" as == Nothing then rx else ry)
    pure [rectangle x y w h rx' ry' | w > 0 && h > 0]
  _ -> either (const (Left "path with commands other than M, L, H, V, C and Z")) Right (P.parse pathData "" (fromMaybe "" (lookup "d" as)))
  where
    len = readLength name as

-- | Four straight sides, or sides joined by cubic quarter-ellipse approximations.
rectangle :: Double -> Double -> Double -> Double -> Double -> Double -> Subpath
rectangle x y w h rx ry
  | rx == 0 || ry == 0 = (a, zipWith straight [a, b, c, d] [b, c, d, a])
  | otherwise =
      ( p0
      , [ straight p0 p1
        , (p1 + V2 (k * rx) 0, p2 - V2 0 (k * ry), p2)
        , straight p2 p3
        , (p3 + V2 0 (k * ry), p4 + V2 (k * rx) 0, p4)
        , straight p4 p5
        , (p5 - V2 (k * rx) 0, p6 + V2 0 (k * ry), p6)
        , straight p6 p7
        , (p7 - V2 0 (k * ry), p0 - V2 (k * rx) 0, p0)
        ]
      )
  where
    a = V2 x y; b = V2 (x + w) y; c = V2 (x + w) (y + h); d = V2 x (y + h)
    p0 = a + V2 rx 0; p1 = b - V2 rx 0
    p2 = b + V2 0 ry; p3 = c - V2 0 ry
    p4 = c - V2 rx 0; p5 = d + V2 rx 0
    p6 = d - V2 0 ry; p7 = a + V2 0 ry
    k = 4 * (sqrt 2 - 1) / 3

straight :: V2 Double -> V2 Double -> (V2 Double, V2 Double, V2 Double)
straight p q = (p + (q - p) ^/ 3, p + (q - p) ^* (2 / 3), q)

guardShape :: Bool -> String -> Either String ()
guardShape ok why = if ok then Right () else Left why

-- | Unitless and pixel lengths, with finite coordinates for the renderer.
readLength :: String -> Attributes -> String -> Double -> Either String Double
readLength element as attribute fallback = case lookup attribute as of
  Nothing -> Right fallback
  Just value -> case P.parseMaybe (space *> number <* optional (string "px") <* space) value of
    Just q | let n = fromRational q, not (isInfinite n || isNaN n) -> Right n
    _ -> Left (element <> " with an unreadable " <> attribute)

-- | A number, exactly. Exponents beyond a thousand aren't read: no drawing
-- needs them, and their powers of ten take long to build.
number :: Parser Rational
number = do
  sign <- option 1 (-1 <$ char '-' <|> 1 <$ char '+')
  whole <- many digitChar
  part <- option "" (char '.' *> many digitChar)
  guard (not (null whole && null part))
  e <- option 0 (oneOf ("eE" :: String) *> (option id (negate <$ char '-' <|> id <$ char '+') <*> (read <$> some digitChar)))
  guard (abs e <= 1000)
  pure (sign * fromInteger (read ('0' : whole <> part)) * 10 ^^ (e - fromIntegral (length part) :: Integer))

separator :: Parser ()
separator = skipMany (satisfy isSpace <|> char ',')

-- | translate(X Y), scale(S), or both, as a stroke's origin and scale.
transform :: Parser (Pos, Double)
transform = do
  separator
  t <- option 0 (string "translate(" *> (V2 <$> (separator *> number) <*> (separator *> number)) <* separator <* char ')')
  separator
  s <- option 1 (string "scale(" *> separator *> number <* separator <* char ')')
  separator <* eof
  pure (t, fromRational s)

-- | Path data as subpaths: where each starts, and its curves' other three
-- points. A line is a straight curve, and Z one back to where it began.
pathData :: Parser [Subpath]
pathData = do
  commands <- separator *> many ((,) <$> oneOf ("MmLlHhVvCcZz" :: String) <* separator <*> many (fromRational <$> number <* separator)) <* eof
  maybe (fail "path data") (pure . finish) (foldM step (0, 0, []) commands)
  where
    finish (_, _, done) = reverse [(s, reverse cs) | (s, cs) <- done]
    -- The current point, where the subpath began, and the subpaths so far,
    -- newest first and their curves too.
    step (p, start, done) (c, ns) = case (toUpper c, ns) of
      ('M', x : y : rest) -> let q = to x y in step (q, q, (q, []) : done) (if isLower c then 'l' else 'L', rest)
      ('L', x : y : rest) -> step (line (to x y)) (c, rest)
      ('H', x : rest) -> step (line (V2 (if isLower c then px + x else x) py)) (c, rest)
      ('V', y : rest) -> step (line (V2 px (if isLower c then py + y else y))) (c, rest)
      ('C', x1 : y1 : x2 : y2 : x : y : rest) -> step (add (to x1 y1, to x2 y2, to x y)) (c, rest)
      ('Z', []) -> Just (line start)
      (u, []) | u /= 'M' -> Just (p, start, done)
      _ -> Nothing
      where
        V2 px py = p
        to x y = if isLower c then p + V2 x y else V2 x y
        line q = add (straight p q)
        add curve@(_, _, q) = (q, start, case done of (s, cs) : rest -> (s, curve : cs) : rest; [] -> [(p, [curve])])
