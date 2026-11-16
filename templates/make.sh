#!/bin/sh
# Make the templates in this directory in Sketch's minimal SVG subset:
# lined paper and strips of panels. Their lines are locked,
# for the eraser to leave alone. Start a drawing from one, saving elsewhere:
#
#   sketch templates/lined.svg -o notes.svg
set -e
cd "$(dirname "$0")"

# A 640 by 900 page of lines 30 apart, with a margin line.
{
  echo '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 900">'
  echo '<g fill="none" stroke-width="1" stroke-linecap="round" data-locked="true">'
  y=90
  while [ "$y" -le 870 ]; do
    echo "<line stroke=\"#a8bfe0\" x1=\"40\" y1=\"$y\" x2=\"600\" y2=\"$y\"/>"
    y=$((y + 30))
  done
  echo '<line stroke="#e8a0a0" x1="90" y1="40" x2="90" y2="880"/>'
  echo '</g>'
  echo '</svg>'
} > lined.svg

# Three strips of panels on a 640 by 900 page: two, three and one panel wide,
# 260 high, with gutters of 10.
{
  echo '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 900">'
  echo '<g fill="none" stroke="#909090" stroke-width="2" stroke-linejoin="round" data-locked="true">'
  top=40
  for panels in 2 3 1; do
    width=$(((560 - 10 * (panels - 1)) / panels))
    x=40
    i=0
    while [ "$i" -lt "$panels" ]; do
      right=$((x + width))
      bottom=$((top + 260))
      echo "<path d=\"M $x $top C $x $top $right $top $right $top $right $top $right $bottom $right $bottom $right $bottom $x $bottom $x $bottom $x $bottom $x $top $x $top\"/>"
      x=$((x + width + 10))
      i=$((i + 1))
    done
    top=$((top + 270))
  done
  echo '</g>'
  echo '</svg>'
} > panels.svg
