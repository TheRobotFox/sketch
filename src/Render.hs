{-# LANGUAGE MultilineStrings #-}

-- | Drawing the model with OpenGL. The CPU cuts strokes into straight
-- pieces, and a shader works out how much of each pixel a piece's ink
-- covers: the discs along it, its radius going evenly from end to end. A
-- stroke's pieces keep the most coverage where they overlap, in the
-- target's alpha, and its colour then goes through that alpha.
module Render (Renderer, newRenderer, render) where

import Canvas
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as B
import Data.ByteString.Builder (floatLE, toLazyByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (toList)
import Data.IORef
import Data.Maybe (mapMaybe)
import Foreign.Ptr (nullPtr, plusPtr)
import Graphics.GL.Core33 (glVertexAttribDivisor)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL qualified as GL
import Linear
import Model (Model (..))
import Stroke (Box)

data Renderer = Renderer
  { pieces :: GL.BufferObject
  , layer, frame :: Target -- ^ the finished strokes as last drawn, and those plus the stroke being drawn
  , uniforms :: String -> GL.UniformLocation
  , drawn :: IORef (Maybe (V2 Int, View, Int)) -- ^ the size, view and edits the layer shows
  }

data Target = Target GL.FramebufferObject GL.TextureObject

-- | Set up drawing in the current OpenGL context.
newRenderer :: IO Renderer
newRenderer = do
  program <- GL.createProgram
  forM_ [(GL.VertexShader, vertexShader), (GL.FragmentShader, fragmentShader)] \(kind, source) -> do
    s <- GL.createShader kind
    GL.shaderSourceBS s $= B8.pack source
    GL.compileShader s
    ok <- GL.get (GL.compileStatus s)
    unless ok (GL.get (GL.shaderInfoLog s) >>= fail)
    GL.attachShader program s
  GL.linkProgram program
  linked <- GL.get (GL.linkStatus program)
  unless linked (GL.get (GL.programInfoLog program) >>= fail)
  GL.currentProgram $= Just program
  locations <- traverse (\n -> (n,) <$> GL.uniformLocation program n) ["size", "paint", "box", "color"]
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  pieces <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just pieces
  -- Each piece is 20 floats, as five vec4s.
  forM_ [0 .. 4] \i -> do
    GL.vertexAttribArray (GL.AttribLocation i) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation i) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 80 (nullPtr `plusPtr` (16 * fromIntegral i)))
    glVertexAttribDivisor i 1
  GL.blend $= GL.Enabled
  layer <- target
  frame <- target
  drawn <- newIORef Nothing
  pure Renderer {pieces, layer, frame, uniforms = \n -> maybe (error n) id (lookup n locations), drawn}
  where
    target = do
      t <- GL.genObjectName
      GL.textureBinding GL.Texture2D $= Just t
      GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
      f <- GL.genObjectName
      GL.bindFramebuffer GL.Framebuffer $= f
      GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D t 0
      pure (Target f t)

-- | Draw a frame in a window this many pixels in size. The finished strokes
-- are drawn again only when they or the view change.
render :: Renderer -> V2 Int -> Model -> IO ()
render r size m = do
  let key = (size, m.view, m.edits)
  before <- readIORef r.drawn
  when (Just key /= before) do
    when (fmap (\(s, _, _) -> s) before /= Just size) $
      forM_ [r.layer, r.frame] \(Target _ t) -> do
        GL.textureBinding GL.Texture2D $= Just t
        GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D w h) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
    onto r.layer do
      -- White as viewers show SVG, with no ink in its alpha.
      GL.clearColor $= GL.Color4 1 1 1 0
      GL.clear [GL.ColorBuffer]
      ink r window [(s.color, shown window m.view s) | s <- toList m.strokes]
    writeIORef r.drawn (Just key)
  copy r.layer (Just r.frame)
  onto r.frame $ ink r window [(d.color, live window m.view d) | Just d <- [m.drawing]]
  copy r.frame Nothing
  where
    V2 w h = fromIntegral <$> size
    window = fromIntegral <$> size
    onto (Target f _) action = do
      GL.bindFramebuffer GL.Framebuffer $= f
      GL.viewport $= (GL.Position 0 0, GL.Size w h)
      action
    copy (Target f _) to = do
      GL.bindFramebuffer GL.ReadFramebuffer $= f
      GL.bindFramebuffer GL.DrawFramebuffer $= maybe GL.defaultFramebufferObject (\(Target f' _) -> f') to
      GL.blitFramebuffer (GL.Position 0 0) (GL.Position w h) (GL.Position 0 0) (GL.Position w h) [GL.ColorBuffer'] GL.Nearest

-- | Ink strokes, oldest first, onto the target, each in its colour.
ink :: Renderer -> V2 Double -> [(Color, [Piece])] -> IO ()
ink r window strokes = do
  GL.uniform (r.uniforms "size") $= (realToFrac <$> GL.Vector2 wx wy :: GL.Vector2 GL.GLfloat)
  forM_ strokes \(Color red green blue, s) -> case mapMaybe (encode window) s of
    [] -> pure ()
    ps -> do
      let (V2 lx ly, V2 hx hy) = foldr1 (\(l, h) (l', h') -> (liftA2 min l l', liftA2 max h h')) (map snd ps)
      B.useAsCStringLen (B.toStrict (toLazyByteString (foldMap (foldMap floatLE . fst) ps))) \(p, n) ->
        GL.bufferData GL.ArrayBuffer $= (fromIntegral n, p, GL.StreamDraw)
      -- Coverage, keeping the most, into alpha alone.
      GL.blendEquationSeparate $= (GL.FuncAdd, GL.Max)
      GL.blendFuncSeparate $= ((GL.Zero, GL.One), (GL.One, GL.One))
      GL.uniform (r.uniforms "paint") $= (0 :: GL.GLint)
      GL.drawArraysInstanced GL.TriangleStrip 0 4 (fromIntegral (length ps))
      -- The colour through that alpha, over the box the pieces reach, which
      -- leaves the alpha 0 again.
      GL.blendEquationSeparate $= (GL.FuncAdd, GL.FuncAdd)
      GL.blendFuncSeparate $= ((GL.DstAlpha, GL.OneMinusDstAlpha), (GL.Zero, GL.Zero))
      GL.uniform (r.uniforms "paint") $= (1 :: GL.GLint)
      GL.uniform (r.uniforms "box") $= (realToFrac <$> GL.Vector4 lx ly hx hy :: GL.Vector4 GL.GLfloat)
      GL.uniform (r.uniforms "color") $= (GL.Color4 (byte red) (byte green) (byte blue) 1 :: GL.Color4 GL.GLfloat)
      GL.drawArraysInstanced GL.TriangleStrip 0 4 1
  where
    V2 wx wy = window
    byte v = fromIntegral v / 255

-- | A piece as the shader takes it, with the box it inks in the window, if
-- any: each part of the hull's distance worked out in Doubles at the
-- window's corner (0, 0), as the shader's 32-bit floats can only add small
-- steps to that. It measures from the first end, which should be the nearer.
encode :: V2 Double -> Piece -> Maybe ([Float], Box)
encode window ((a0, ra0), (b0, rb0))
  | not (all (\v -> not (isNaN v || isInfinite v)) numbers) || or (liftA2 (>=) lo hi) = Nothing
  | otherwise = Just (map realToFrac (numbers <> [lx, ly, hx, hy]), (lo, hi))
  where
    ((a, ra), (b, rb)) = if quadrance (b0 - window / 2) < quadrance (a0 - window / 2) then ((b0, rb0), (a0, ra0)) else ((a0, ra0), (b0, rb0))
    h = norm (b - a)
    -- One disc holds the other, which makes the hull.
    one = h <= abs (ra - rb)
    (c0, r0) = if one && rb > ra then (b, rb) else (a, ra)
    (c1, r1) = if one then (c0, r0) else (b, rb)
    u@(V2 ux uy) = if one || h == 0 then V2 1 0 else (b - a) ^/ h
    s = if one then 0 else (ra - rb) / h
    c = sqrt (1 - s * s)
    V2 across along = V2 (crossZ u (negate c0)) (dot u (negate c0))
    -- |c|^2 - r^2, in a form that doesn't lose it.
    e p r = (norm p - r) * (norm p + r)
    (V2 dax day, V2 dbx dby) = (negate c0, negate c1)
    numbers = [dax, day, dbx, dby, ux, uy, across, along, e c0 r0, e c1 r1, r0, r1, s, if one then 0 else h, c * across + s * along - r0, s * along - c * across - r0]
    reach = pure (max ra rb + 1)
    lo@(V2 lx ly) = liftA2 max 0 (liftA2 min a b - reach)
    hi@(V2 hx hy) = liftA2 min window (liftA2 max a b + reach)

vertexShader :: String
vertexShader =
  """
  #version 330 core
  layout(location = 0) in vec4 ends;  // the window's corner (0, 0) less each end
  layout(location = 1) in vec4 frame; // the unit vector from the first end, and the corner across and along it
  layout(location = 2) in vec4 discs; // |corner - end|^2 - radius^2 for each end, and each radius
  layout(location = 3) in vec4 side;  // s, the length, and the corner's distance to each side's line
  layout(location = 4) in vec4 quad;  // the box the piece inks
  uniform vec2 size;
  uniform bool paint;
  uniform vec4 box;
  flat out vec4 vEnds, vFrame, vDiscs, vSide;
  void main() {
    vec4 b = paint ? box : quad;
    vec2 p = mix(b.xy, b.zw, vec2(gl_VertexID & 1, gl_VertexID >> 1));
    gl_Position = vec4(p.x / size.x * 2.0 - 1.0, 1.0 - p.y / size.y * 2.0, 0.0, 1.0);
    vEnds = ends; vFrame = frame; vDiscs = discs; vSide = side;
  }
  """

-- | How far the pixel's centre lies outside the hull, with s = (ra - rb) / h
-- (Inigo Quilez's uneven capsule).
fragmentShader :: String
fragmentShader =
  """
  #version 330 core
  flat in vec4 vEnds, vFrame, vDiscs, vSide;
  uniform vec2 size;
  uniform bool paint;
  uniform vec4 color;
  out vec4 fragColor;
  // |p - c| - r, for c = -d, from e = |d|^2 - r^2.
  float disc(vec2 p, vec2 d, float e, float r) {
    return (e + dot(p, 2.0 * d + p)) / (length(d + p) + r);
  }
  void main() {
    if (paint) { fragColor = color; return; }
    vec2 p = vec2(gl_FragCoord.x, size.y - gl_FragCoord.y);
    vec2 u = vFrame.xy, n = vec2(-u.y, u.x);
    float x = vFrame.z + dot(p, n), y = vFrame.w + dot(p, u);
    float s = vSide.x, c = sqrt(max(0.0, 1.0 - s * s));
    float k = c * y - s * abs(x);
    float d = k < 0.0 ? disc(p, vEnds.xy, vDiscs.x, vDiscs.z)
      : k > c * vSide.y ? disc(p, vEnds.zw, vDiscs.y, vDiscs.w)
      : (x >= 0.0 ? vSide.z + c * dot(p, n) : vSide.w - c * dot(p, n)) + s * dot(p, u);
    fragColor = vec4(0.0, 0.0, 0.0, clamp(0.5 - d, 0.0, 1.0));
  }
  """
