-- | A window with an OpenGL 3.3 context, and its input as 'Event's, through
-- SDL3, which knows pens.
module Window (Window, withWindow, pollEvents, windowSize, setTitle, swap) where

import Control.Exception (bracket, bracket_)
import Control.Monad (unless, void)
import Data.Bits (zeroBits, (.&.), (.|.))
import Data.Maybe (catMaybes)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Linear (V2 (..))
import Model (Event (..))
import SDL3.Sys qualified as SDL3

newtype Window = Window (Ptr SDL3.SDL_Window)

withWindow :: String -> (Window -> IO a) -> IO a
withWindow name action = bracket_ start SDL3.quit $ bracket create destroy \(w, _) -> do
  -- Pace frames to the display; fine if the system can't.
  void (SDL3.glSetSwapInterval 1)
  action (Window w)
  where
    start = do
      -- Keypad keys as the keys they copy, so its digits, + and - work too.
      void $ withCString "SDL_KEYCODE_OPTIONS" \h -> withCString "hide_numpad,french_numbers,latin_letters" \v ->
        SDL3.setHint (ConstPtr h) (ConstPtr v)
      check "SDL_Init" (SDL3.init SDL3.SDL_INIT_VIDEO)
      mapM_ (\(a, v) -> SDL3.glSetAttribute a v) [(SDL3.SDL_GL_CONTEXT_MAJOR_VERSION, 3), (SDL3.SDL_GL_CONTEXT_MINOR_VERSION, 3), (SDL3.SDL_GL_CONTEXT_PROFILE_MASK, fromIntegral SDL3.SDL_GL_CONTEXT_PROFILE_CORE)]
    create = do
      w <- withCString name \n -> SDL3.createWindow (ConstPtr n) 640 480 (SDL3.SDL_WINDOW_OPENGL .|. SDL3.SDL_WINDOW_RESIZABLE)
      check "SDL_CreateWindow" (pure (w /= nullPtr))
      context <- SDL3.glCreateContext w
      check "SDL_GL_CreateContext" (SDL3.glMakeCurrent w context)
      pure (w, context)
    destroy (w, context) = SDL3.glDestroyContext context >> SDL3.destroyWindow w

check :: String -> IO Bool -> IO ()
check function action = do
  ok <- action
  unless ok (SDL3.getError >>= peekCString . unConstPtr >>= \e -> fail (function <> ": " <> e))

windowSize :: Window -> IO (V2 Int)
windowSize (Window w) = alloca \x -> alloca \y -> do
  void (SDL3.getWindowSizeInPixels w x y)
  V2 <$> (fromIntegral <$> peek x) <*> (fromIntegral <$> peek y)

setTitle :: Window -> String -> IO ()
setTitle (Window w) t = void (withCString t (SDL3.setWindowTitle w . ConstPtr))

swap :: Window -> IO ()
swap (Window w) = void (SDL3.glSwapWindow w)

-- | Every pending event, without waiting.
pollEvents :: IO [Event]
pollEvents = alloca \ptr ->
  let drain = do
        pending <- SDL3.pollEvent ptr
        if pending then (:) <$> decode ptr <*> drain else pure []
   in catMaybes <$> drain

decode :: Ptr SDL3.SDL_Event -> IO (Maybe Event)
decode ptr = do
  -- A union: the type tag first, then the matching part.
  tag <- peek (castPtr ptr :: Ptr SDL3.SDL_EventType)
  e <- peek ptr
  let at x y = V2 (realToFrac x) (realToFrac y)
      held mask = e.motion.state .&. mask /= zeroBits
      -- SDL also reports a pen as the mouse, its tip as the left button.
      notPen which = which /= SDL3.SDL_PEN_MOUSEID
      leftButton = e.button.button == 1 && notPen e.button.which
  pure case tag of
    SDL3.SDL_EVENT_QUIT -> Just Quit
    SDL3.SDL_EVENT_PEN_DOWN -> Just (PenDown (e.ptouch.eraser /= 0) (at e.ptouch.x e.ptouch.y))
    SDL3.SDL_EVENT_PEN_UP -> Just PenUp
    SDL3.SDL_EVENT_PEN_MOTION -> Just (PenMove (at e.pmotion.x e.pmotion.y))
    SDL3.SDL_EVENT_PEN_AXIS
      | e.paxis.axis == SDL3.SDL_PEN_AXIS_PRESSURE -> Just (PenPressure (realToFrac e.paxis.value) (at e.paxis.x e.paxis.y))
    SDL3.SDL_EVENT_MOUSE_WHEEL -> Just (Wheel (realToFrac e.wheel.y) (at e.wheel.mouse_x e.wheel.mouse_y))
    SDL3.SDL_EVENT_MOUSE_MOTION
      | held (SDL3.SDL_BUTTON_RMASK .|. SDL3.SDL_BUTTON_MMASK) -> Just (Drag (at e.motion.xrel e.motion.yrel))
      | held SDL3.SDL_BUTTON_LMASK && notPen e.motion.which -> Just (MouseMove (at e.motion.x e.motion.y))
    SDL3.SDL_EVENT_MOUSE_BUTTON_DOWN | leftButton -> Just (MouseDown (at e.button.x e.button.y))
    SDL3.SDL_EVENT_MOUSE_BUTTON_UP | leftButton -> Just MouseUp
    -- Keys that type something have their character's code point.
    SDL3.SDL_EVENT_KEY_DOWN | e.key.repeat == 0, code <- e.key.key.unwrap, code < 0x110000 ->
      Just ((if e.key.mod .&. SDL3.SDL_KMOD_CTRL /= zeroBits then Ctrl else Key) (toEnum (fromIntegral code)))
    _ -> Nothing
