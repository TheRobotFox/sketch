{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- A port of SDL's examples/pen/01-drawing-lines: reads pen/stylus input and
-- draws lines, darker for harder pressure. Strokes go onto a canvas that keeps
-- them between frames, so each frame is a single canvas copy instead of a
-- replay of every stroke.

import Control.Monad (foldM, unless)
import Data.Foldable (for_)
import Numeric (showFFloat)
import Sketch.SDL

data Pen = Pen
  { pressure :: Float
  , previous :: Maybe (Float, Float) -- last position while touching
  , tiltX :: Float
  , tiltY :: Float
  }

black, gray :: Color
black = RGBA 0 0 0 1
gray = RGBA 0.4 0.4 0.4 1

main :: IO ()
main =
  withSDL $
    withWindow "sketch" 640 480 \renderer ->
      withCanvas renderer \canvas -> do
        onCanvas renderer canvas $
          clear renderer gray
        loop renderer canvas Pen {pressure = 0, previous = Nothing, tiltX = 0, tiltY = 0}

-- | Handle pending events, then draw a frame, until the window is closed.
loop :: Renderer -> Canvas -> Pen -> IO ()
loop renderer canvas pen = do
  events <- pollEvents
  unless (Quit `elem` events) do
    pen' <- foldM (handleEvent renderer canvas) pen events
    drawFrame renderer canvas pen'
    loop renderer canvas pen'

-- | Like the upstream example, this treats all pen input as one device.
handleEvent :: Renderer -> Canvas -> Pen -> Event -> IO Pen
handleEvent renderer canvas pen event = case event of
  PenMotion x y
    -- Pressure above zero means the pen is definitely touching.
    | pen.pressure > 0 -> do
        for_ pen.previous \from ->
          -- Alpha follows pressure, so lighter presses draw fainter lines.
          onCanvas renderer canvas $
            drawLine renderer (RGBA 0 0 0 pen.pressure) from (x, y)
        pure pen {previous = Just (x, y)}
    | otherwise -> pure pen {previous = Nothing}
  PenAxis Pressure value -> pure pen {pressure = value}
  PenAxis TiltX value -> pure pen {tiltX = value}
  PenAxis TiltY value -> pure pen {tiltY = value}
  Quit -> pure pen

drawFrame :: Renderer -> Canvas -> Pen -> IO ()
drawFrame renderer canvas pen = do
  clear renderer black
  drawCanvas renderer canvas
  drawText renderer black (0, 8) $
    "Tilt: " <> showFFloat (Just 2) pen.tiltX " " <> showFFloat (Just 2) pen.tiltY ""
  present renderer
