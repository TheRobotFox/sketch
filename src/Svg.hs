{-# LANGUAGE OverloadedStrings #-}

-- | Sketch's own SVG paths round-trip exactly. Templates may also use
-- plain lines and groups inheriting stroke colour, width and the lock flag.
module Svg (Drawing (..), document, parse) where

import Canvas (Color (..), Ink (..), Pos, View (..), readColor)
import Control.Monad (guard, unless)
import Data.ByteString.Builder
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (isJust)
import Data.Ratio (denominator, numerator)
import Data.Void (Void)
import Linear
import Numeric (showHex)
import Stroke (Stroke, bounds, curves, fromCurves, unionBounds)
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
        let (lo, hi) = foldr1 unionBounds bs
            margin = pure (maximum (hi - lo) / 40)
         in toList (lo - margin) <> toList (hi - lo + 2 * margin)
    place i q = fmap fromRational i.origin + q ^* i.scale

path :: Ink Stroke -> Builder
path i =
  "<path"
    <> (if i.origin == 0 && i.scale == 1 then "" else " transform=\"translate(" <> decimal ox <> " " <> decimal oy <> ") scale(" <> double i.scale <> ")\"")
    <> (if i.locked then " data-locked=\"true\"" else "")
    <> " stroke=\"" <> hex i.color <> "\" stroke-width=\"" <> double (2 * if constant then r0 else sum radii / fromIntegral (length radii)) <> "\""
    <> " d=\"M" <> xy a <> "C" <> cubic first <> foldMap (\c -> " " <> cubic c) rest <> "\""
    <> (if constant then "" else " data-r=\"" <> spaced radii <> "\"")
    <> "/>\n"
  where
    V2 ox oy = i.origin
    first@(V4 a _ _ _) :| rest = i.stroke.curves
    radii = r0 : [r | V4 _ b c d <- toList i.stroke.curves, V3 _ _ r <- [b, c, d]]
    V3 _ _ r0 = a
    constant = all (== r0) radii
    xy (V3 px py _) = double px <> " " <> double py
    cubic (V4 _ b c d) = xy b <> " " <> xy c <> " " <> xy d
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
parse text = case dropWhile (\case TagOpen "svg" _ -> False; _ -> True) (parseTags text) of
  TagOpen _ root : rest ->
    let (inks, skipped) = if decorated root then ([], ["styled or transformed svg"]) else walk [inherited root] [] rest
        view = do
          [x, y, z] <- traverse (P.parseMaybe number) . words =<< lookup "data-view" root
          guard (z >= 1e-300 && z <= 1e300)
          pure (View (V2 x y) (fromRational z))
     in Just Drawing {view, strokes = inks, skipped}
  _ -> Nothing

walk :: [Attributes] -> [String] -> [Tag String] -> ([Ink Stroke], [String])
walk groups skipped = \case
  [] -> ([], skipped)
  TagOpen "g" as : rest
    | decorated as -> walk groups ("styled or transformed group" : skipped) (after "g" rest)
    | otherwise -> walk (inherited as : groups) skipped rest
  TagClose "g" : rest -> walk (drop 1 groups) skipped rest
  TagOpen name as : rest
    | name `elem` ["path", "line"] -> case readShape name (as <> concat groups) of
        Right ink -> let (inks, s) = walk groups skipped rest in (ink <> inks, s)
        Left why -> walk groups (why : skipped) rest
    | name `elem` ["title", "desc", "metadata"] -> walk groups skipped (after name rest)
    | otherwise -> walk groups (name : skipped) (after name rest)
  _ : rest -> walk groups skipped rest

inherited :: Attributes -> Attributes
inherited = filter (\(key, _) -> key `elem` ["stroke", "stroke-width", "data-locked"])

decorated :: Attributes -> Bool
decorated as = any (isJust . (`lookup` as)) ["style", "transform"]

-- | Skip an unsupported element together with its contents.
after :: String -> [Tag String] -> [Tag String]
after name = go (1 :: Int)
  where
    go 0 ts = ts
    go n (TagOpen m _ : ts) | m == name = go (n + 1) ts
    go n (TagClose m : ts) | m == name = go (n - 1) ts
    go n (_ : ts) = go n ts
    go _ [] = []

readShape :: String -> Attributes -> Either String [Ink Stroke]
readShape name as
  | isJust (lookup "style" as) = Left (name <> " with style")
  | otherwise = case lookup "stroke" as of
      Nothing -> Right []
      Just "none" -> Right []
      Just stroke -> do
        color <- maybe (bad "stroke") Right (readColor stroke)
        width <- get "stroke-width" value 1
        unless (width >= 0) (bad "stroke-width")
        (origin, scale) <- get "transform" transform (0, 1)
        points <- if name == "path" then get "d" pathData [] else do
          a <- V2 <$> get "x1" value 0 <*> get "y1" value 0
          b <- V2 <$> get "x2" value 0 <*> get "y2" value 0
          pure [a, a, b, b]
        radii <- get "data-r" (some value) (replicate (length points) (width / 2))
        unless (length radii == length points) (bad "data-r")
        let at3 (V2 x y) r = V3 x y r
            join (a : b : c : rest@(d : _)) = V4 a b c d : join rest
            join _ = []
        pure [Ink origin scale color (lookup "data-locked" as == Just "true") (fromCurves cs) | width > 0, Just cs <- [nonEmpty (join (zipWith at3 points radii))]]
  where
    bad key = Left (name <> " with an unreadable " <> key)
    get key parser fallback = case lookup key as of
      Nothing -> Right fallback
      Just text -> maybe (bad key) Right (P.parseMaybe parser text)

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
separator = skipMany (spaceChar <|> char ',')

-- | Finite local coordinates; canvas origins and the view stay rational.
value :: Parser Double
value = do
  n <- fromRational <$> (separator *> number <* separator)
  guard (not (isNaN n || isInfinite n))
  pure n

-- | The placement written by Sketch, when it isn't the identity.
transform :: Parser (Pos, Double)
transform = do
  t <- string "translate(" *> (V2 <$> (separator *> number) <*> (separator *> number)) <* separator <* char ')'
  s <- separator *> string "scale(" *> value <* char ')'
  guard (s > 0)
  pure (t, s)

-- | One absolute move followed by cubic curves, with repeated C optional.
pathData :: Parser [V2 Double]
pathData = do
  a <- char 'M' *> xy
  cs <- concat <$> some (char 'C' *> some (sequenceA (V3 xy xy xy)))
  pure (a : concatMap toList cs)
  where
    xy = V2 <$> value <*> value
