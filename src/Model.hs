-- | What sketch holds and how input changes it, with no IO: the window
-- feeds 'Event's to 'update', and draws what the 'Model' says.
module Model
  ( Settings (..)
  , Model (..)
  , Event (..)
  , initial
  , update
  , title
  ) where

import Canvas (Color, Ink, View (..), addSample, drag, finish, start, wipe, zoomAt)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Linear (V2 (..), norm, (^*))
import Numeric (showEFloat, showFFloat)
import Stroke (Point, Stroke)

data Settings = Settings
  { width :: Double -- ^ the pen's width at full pressure, in window pixels
  , smoothing :: Double -- ^ how far a stroke may stray from the pen to smooth it, in window pixels
  , palette :: [Color] -- ^ what keys 1 to 9 pick
  }

data Model = Model
  { settings :: Settings
  , view :: View
  , strokes :: Seq (Ink Stroke) -- ^ oldest first
  , edits :: Int -- ^ how many times the strokes have changed
  , past, future :: [(Seq (Ink Stroke), View)] -- ^ strokes to go back to, newest first, and forward to again, each with the view its change was made in
  , drawing :: Maybe (Ink [Point]) -- ^ the stroke being drawn, its samples newest first
  , erasing :: Maybe (Seq (Ink Stroke), Int, V2 Double) -- ^ while the eraser touches: the strokes before, the edits then, and where it last was
  , pressure :: Double -- ^ the pen's, from 0 to 1
  , color :: Int -- ^ which of the palette's colours the pen draws in, from 1, or 0 to erase
  , penWidth :: Double -- ^ the pen's width at full pressure, in window pixels
  }

data Event
  = -- | The pen touched down, with its eraser end if True.
    PenDown Bool (V2 Double)
  | PenMove (V2 Double)
  | PenPressure Double (V2 Double)
  | PenUp
  | -- | The left button, or a touchpad, which draws at half pressure.
    MouseDown (V2 Double)
  | MouseMove (V2 Double)
  | MouseUp
  | -- | Steps of the wheel, away from the user positive, at a window position.
    Wheel Double (V2 Double)
  | -- | The right or middle button, or a pen's barrel button, dragged this far.
    Drag (V2 Double)
  | -- | The character a key types, alone or with Ctrl.
    Key Char
  | Ctrl Char
  | Quit
  deriving (Eq, Show)

initial :: Settings -> Model
initial s = Model {settings = s, view = View 0 1, strokes = mempty, edits = 0, past = [], future = [], drawing = Nothing, erasing = Nothing, pressure = 0, color = 1, penWidth = s.width}

update :: Event -> Model -> Model
update event m = case event of
  PenDown end s
    | end || m.color == 0 -> rub s (startRub s m)
    | otherwise -> press s (begin m)
  PenMove s -> moved s (press s) m
  PenPressure p s -> moved s (press s) m {pressure = p}
  MouseDown s
    | m.color == 0 -> rub s (startRub s m)
    | otherwise -> sample 0.5 s (begin m)
  MouseMove s -> moved s (sample 0.5 s) m
  -- A stroke keeps the zoom it began at, and erasing where it has been.
  Wheel steps s | idle -> m {view = zoomAt s (1.1 ** steps) m.view}
  -- A pen's barrel button pans, and its tip may touch meanwhile.
  Drag d -> (stop m {drawing = Nothing}) {view = drag d m.view}
  -- Back to the strokes before the last change, in the view it was made
  -- in, and forward again.
  Ctrl 'z' | idle, (s, v) : rest <- m.past -> m {strokes = s, view = v, past = rest, future = (m.strokes, v) : m.future, edits = m.edits + 1}
  Ctrl 'y' | idle, (s, v) : rest <- m.future -> m {strokes = s, view = v, future = rest, past = (m.strokes, v) : m.past, edits = m.edits + 1}
  Key c
    | Just i <- elemIndex c (take (length m.settings.palette + 1) "0123456789") -> m {color = i}
    | c `elem` ("+=" :: String) -> m {penWidth = m.penWidth * 1.25}
    | c `elem` ("-_" :: String) -> m {penWidth = m.penWidth / 1.25}
  _ | event `elem` [PenUp, MouseUp, Quit] -> stop m
  _ -> m
  where
    idle = isNothing m.drawing && isNothing m.erasing
    begin m' = m' {drawing = Just (start m'.view (m'.settings.palette !! (m'.color - 1)))}
    -- Systems send a pen's touch, moves and pressure in any order, so each
    -- adds a sample while it presses.
    press s m' = if m'.pressure > 0 then sample m'.pressure s m' else m'
    sample p s m' = m' {drawing = addSample m'.view s (m'.penWidth * p) <$> m'.drawing}
    moved s f m' = if isJust m'.erasing then rub s m' else f m'

-- | Start erasing at a window position.
startRub :: V2 Double -> Model -> Model
startRub s m = m {drawing = Nothing, erasing = Just (m.strokes, m.edits, s)}

-- | Erase from where the eraser last was to here, its discs spaced half its
-- radius apart.
rub :: V2 Double -> Model -> Model
rub s m = case m.erasing of
  Nothing -> m
  Just (before, since, from) ->
    let w = 5 * m.penWidth * m.pressure
        n = ceiling (norm (s - from) / (w / 2)) :: Int
        path = from :| [from + (s - from) ^* (fromIntegral i / fromIntegral n) | i <- [1 .. n]]
        cut = fmap (\i -> (i, wipe w m.view path i)) m.strokes
        m' = m {erasing = Just (before, since, s)}
     in if all (isNothing . snd) cut then m' else m' {strokes = foldMap (\(i, c) -> Seq.fromList (fromMaybe [i] c)) cut, edits = m.edits + 1}

-- | Finish the stroke being drawn, or erasing, as a change to go back from.
stop :: Model -> Model
stop m = case (finish m.settings.smoothing =<< m.drawing, m.erasing) of
  (Just s, _) -> m {drawing = Nothing, strokes = m.strokes |> s, edits = m.edits + 1, past = (m.strokes, m.view) : m.past, future = []}
  (_, Just (before, since, _)) | since /= m.edits -> m {erasing = Nothing, past = (before, m.view) : m.past, future = []}
  _ -> m {drawing = Nothing, erasing = Nothing}

-- | What the window's title shows.
title :: Model -> String
title m =
  "sketch  "
    <> (if m.color == 0 then "Eraser " <> show (round (5 * m.penWidth) :: Int) <> " px" else "Color " <> show m.color <> "  Width " <> showFFloat (Just 1) m.penWidth "")
    <> "  Zoom " <> showEFloat (Just 1) m.view.zoom ""
