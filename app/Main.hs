-- | sketch: handwritten notes on an endless canvas, saved as SVG. See the
-- README for how to draw, erase, undo, zoom and pan.
module Main (main) where

import Canvas (Color (..), View (..))
import Control.Exception (IOException, displayException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as B
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Char (digitToInt, isHexDigit)
import Data.Foldable (for_, toList)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence qualified as Seq
import Model
import Options.Applicative
import Render
import Svg qualified
import System.Directory (doesFileExist, renameFile)
import System.Exit (die)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr)
import Window

data Options = Options {file :: FilePath, output :: Maybe FilePath, settings :: Settings}

main :: IO ()
main = do
  o <- execParser (info (options <**> helper) (fullDesc <> progDesc "Draw handwritten notes on an endless canvas."))
  exists <- doesFileExist o.file
  -- Starting from a template, never over a drawing already there.
  for_ o.output \out -> doesFileExist out >>= \taken -> when taken (die (out <> " already exists; open it to draw in it."))
  drawing <-
    if exists
      then maybe (die (o.file <> " isn't an SVG file.")) pure . Svg.parse . B8.unpack =<< B.readFile o.file
      else pure (Svg.Drawing Nothing [] [])
  unless (null drawing.skipped) $
    hPutStrLn stderr ("sketch can't read these in " <> o.file <> ", and saving drops them: " <> intercalate ", " drawing.skipped)
  let path = fromMaybe o.file o.output
      m0 = (initial o.settings) {view = fromMaybe (View 0 1) drawing.view, strokes = Seq.fromList drawing.strokes}
  withWindow "sketch" \w -> do
    r <- newRenderer
    let loop m saved problem shown = do
          events <- pollEvents
          let m' = foldl' (flip update) m events
              quit = Quit `elem` events
          -- Save after every change, once erasing ends, and the view too on
          -- closing.
          problem' <- if m'.edits /= saved && isNothing m'.erasing || quit then save path m' else pure problem
          size <- windowSize w
          render r size m'
          let t = takeFileName path <> "  " <> title m' <> maybe "" ("  NOT SAVED: " <>) problem'
          when (t /= shown) (setTitle w t)
          swap w
          unless quit (loop m' (if isNothing m'.erasing then m'.edits else saved) problem' t)
    loop m0 m0.edits Nothing ""

-- | Save, by way of a temporary file so that a crash can't leave the file
-- half written; why it failed, if it did.
save :: FilePath -> Model -> IO (Maybe String)
save path m = either (\e -> Just (displayException (e :: IOException))) (const Nothing) <$> try do
  B.writeFile (path <> ".tmp") (B.toStrict (toLazyByteString (Svg.document m.view (toList m.strokes))))
  renameFile (path <> ".tmp") path

options :: Parser Options
options =
  Options
    <$> strArgument (metavar "FILE" <> value "sketch.svg" <> showDefault <> help "the drawing to open, or start, and save to")
    <*> optional (strOption (short 'o' <> metavar "NEW.svg" <> help "save to this new file instead, to start from a template"))
    <*> ( Settings
            <$> option positive (long "width" <> metavar "PIXELS" <> value 9 <> showDefault <> help "the pen's width at full pressure")
            <*> option positive (long "smoothing" <> metavar "PIXELS" <> value 2 <> showDefault <> help "how far strokes may stray from the pen, to smooth them")
            <*> option (eitherReader colors) (long "colors" <> metavar "#RGB,..." <> value defaultPalette <> help "the colours keys 1 to 9 pick, as #RGB or #RRGGBB")
        )
  where
    positive = auto >>= \x -> if x > 0 then pure x else readerError "must be positive"
    colors s = case traverse color (splitOn "," s) of
      Just cs@(_ : _) | length cs <= 9 -> Right cs
      _ -> Left "expected 1 to 9 colours like #1f4fd1, separated by commas"
    color ('#' : hex) | all isHexDigit hex = case map digitToInt hex of
      [r, g, b] -> Just (Color (17 * byte r) (17 * byte g) (17 * byte b))
      [r1, r2, g1, g2, b1, b2] -> Just (Color (byte (16 * r1 + r2)) (byte (16 * g1 + g2)) (byte (16 * b1 + b2)))
      _ -> Nothing
    color _ = Nothing
    byte = fromIntegral

defaultPalette :: [Color]
defaultPalette =
  [ Color 0x00 0x00 0x00 -- black
  , Color 0x1f 0x4f 0xd1 -- blue
  , Color 0xd2 0x2b 0x2b -- red
  , Color 0x1e 0x8c 0x3a -- green
  , Color 0xe0 0x7b 0x00 -- orange
  , Color 0x7b 0x3f 0xbf -- purple
  , Color 0x0a 0x8f 0x8f -- teal
  , Color 0x8a 0x5a 0x2b -- brown
  , Color 0x8c 0x8c 0x8c -- grey
  ]
