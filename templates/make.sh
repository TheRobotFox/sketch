#!/bin/sh
# Make the templates in this directory as plain SVG that sketch opens (see
# docs/format.md): lined paper, and strips of panels. Their lines are locked,
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
    echo "<path stroke=\"#a8bfe0\" d=\"M 40 $y H 600\"/>"
    y=$((y + 30))
  done
  echo '<path stroke="#e8a0a0" d="M 90 40 V 880"/>'
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
      echo "<path d=\"M $x $top h $width v 260 h -$width Z\"/>"
      x=$((x + width + 10))
      i=$((i + 1))
    done
    top=$((top + 270))
  done
  echo '</g>'
  echo '</svg>'
} > panels.svg
