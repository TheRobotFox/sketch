-- | What sketch holds and how input changes it, with no IO: the window
-- feeds 'Event's to 'update', and draws what the 'Model' says.
module Model
  ( Settings (..)
  , Model (..)
  , Erasing (..)
  , Event (..)
  , initial
  , update
  , title
  ) where

import Canvas (Color, Ink, View (..), addSample, drag, extent, finish, start, wipe, zoomAt)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Linear (V2)
import Numeric (showEFloat, showFFloat)
import Stroke (Box, Point, Stroke, unionBounds)

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
  , erasing :: Maybe Erasing
  , pressure :: Double -- ^ the pen's, from 0 to 1
  , color :: Int -- ^ which of the palette's colours the pen draws in, from 1, or 0 to erase
  , penWidth :: Double -- ^ the pen's width at full pressure, in window pixels
  }

data Erasing = Erasing
  { before :: Seq (Ink Stroke)
  , since :: Int
  , position :: V2 Double
  , damage :: Maybe Box -- ^ all changed strokes in window pixels, for repainting
  }

data Event
  = -- | The pen touched down, with its eraser end if True.
    PenDown Bool (V2 Double)
  | PenMove (V2 Double)
  | PenPressure Double (V2 Double)
  | PenUp
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
    | end || m.color == 0 -> rub s m {drawing = Nothing, erasing = Just (Erasing m.strokes m.edits s Nothing)}
    | otherwise -> press s (begin m)
  PenMove s -> moved s m
  PenPressure p s -> moved s m {pressure = p}
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
  _ | event `elem` [PenUp, Quit] -> stop m
  _ -> m
  where
    idle = isNothing m.drawing && isNothing m.erasing
    begin m' = m' {drawing = Just (start m'.view (m'.settings.palette !! (m'.color - 1)))}
    -- Systems send a pen's touch, moves and pressure in any order, so each
    -- adds a sample while it presses.
    press s m' = if m'.pressure > 0 then m' {drawing = addSample m'.view s (m'.penWidth * m'.pressure) <$> m'.drawing} else m'
    moved s m' = if isJust m'.erasing then rub s m' else press s m'

-- | Sweep from the previous position. Replace only touched strokes, sharing
-- the rest of the sequence with the current drawing and undo history.
rub :: V2 Double -> Model -> Model
rub s m = case m.erasing of
  Nothing -> m
  Just e | m.pressure <= 0 -> m {erasing = Just e {position = s}}
  Just e ->
    let cut index ink (current, area) = case wipe (5 * m.penWidth * m.pressure) m.view (e.position :| [s]) ink of
          Nothing -> (current, area)
          Just rest -> (Seq.take index current <> Seq.fromList rest <> Seq.drop (index + 1) current, grow (extent m.view ink) area)
        (strokes, damage) = Seq.foldrWithIndex cut (m.strokes, Nothing) m.strokes
        m' = m {erasing = Just e {position = s, damage = maybe e.damage (`grow` e.damage) damage}}
     in if isNothing damage then m' else m' {strokes, edits = m.edits + 1}
  where
    grow area previous = Just (maybe area (unionBounds area) previous)

-- | Finish the stroke being drawn, or erasing, as a change to go back from.
stop :: Model -> Model
stop m = case (finish m.settings.smoothing =<< m.drawing, m.erasing) of
  (Just s, _) -> remember m.strokes done {strokes = m.strokes |> s, edits = m.edits + 1}
  (_, Just e) | e.since /= m.edits -> remember e.before done
  _ -> done
  where
    done = m {drawing = Nothing, erasing = Nothing}
    remember before next = next {past = (before, m.view) : m.past, future = []}

-- | What the window's title shows.
title :: Model -> String
title m =
  "sketch  "
    <> (if m.color == 0 then "Eraser " <> show (round (5 * m.penWidth) :: Int) <> " px" else "Color " <> show m.color <> "  Width " <> showFFloat (Just 1) m.penWidth "")
    <> "  Zoom " <> showEFloat (Just 1) m.view.zoom ""
