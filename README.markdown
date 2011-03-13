A high-level and easy to use graphics library with integrated TikZ
output.

Craftwerk is a high-level 2D vector graphics library for output of
TikZ pictures that can be typeset using (pdf)LaTeX. The TikZ library
and documentation can be found at: http://sourceforge.net/projects/pgf

Craftwerk tries to encapsulate the graphics backend such that figures
can also be rendered with a Cairo backend and quickly displayed in a
Gtk window. The aim is to support TikZ and Cairo seamlessly as
possible, meaning that graphics produced with either backend should
look as similar as possible. Other backends are easily written and the
aim is to provide generic fallback functions for features that are not
natively supported by some backend.