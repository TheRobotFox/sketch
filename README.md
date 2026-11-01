# sketch

Handwritten notes on an endless canvas.

    cabal run sketch -- notes.svg

draws in `notes.svg`, or `sketch.svg` if no file is given; `--help` lists the
options.

Draw with a pen (harder presses draw thicker) or the left mouse button.
Keys 1 to 9 pick a colour, + and - the width. Erase with the pen's eraser
end, or press 0 to erase with its tip or the mouse: the eraser is 16 px
wide and cuts strokes where it touches, but leaves template lines, and ink
over twice its width (zoom out to erase that). Ctrl+Z undoes a stroke or
an erase, in the view it was made in, and Ctrl+Y redoes it. Scroll to zoom,
and drag with the right or middle button, or a pen's barrel button, to pan.

Drawings are plain SVG that any viewer shows: one path per stroke, with its
radius at each point in `data-r` where pen pressure varies it, and the view
in `data-view`. sketch saves to the file it opened after every change, and
the view when it closes. It reads its own files and plain paths (M, L, H, V,
C and Z); on opening anything else, it says what it can't read, which
saving drops. To start from a template, save to a new file:

    cabal run sketch -- templates/lined.svg -o notes.svg

Needs SDL 3 and OpenGL 3.3. `cabal test` checks the geometry, and draws
through the GPU to compare with the CPU, so it needs a GPU too.

## Code

- `Stroke`: strokes as cubic Béziers through (x, y, radius), fitted to pen
  samples, cut into straight pieces where they show, and erased.
- `Canvas`: the endless canvas, exact positions and the view onto it.
- `Model`: the app's state and how input changes it; pure.
- `Window`: SDL3, a window with an OpenGL context and its input as events.
- `Render`: OpenGL; a shader inks each piece's exact, smooth coverage.
- `Svg`: writing drawings and reading them back.
