-- |
-- Module      :  Graphics.Craftwerk.Core
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- A high-level and easy to use graphics library with several backends,
-- currently including TikZ, Cairo (craftwerk-cairo) with Gtk UI (craftwerk-gtk)
-- and Gloss (craftwerk-gloss).


module Graphics.Craftwerk.Core
       (
         module Graphics.Craftwerk.Core.Color
       , module Graphics.Craftwerk.Core.Style
       , module Graphics.Craftwerk.Core.Figure

       , module Graphics.Craftwerk.Core.Driver.Tikz
       , module Graphics.Craftwerk.Core.Driver.Generic
       ) where

import Graphics.Craftwerk.Core.Color
import Graphics.Craftwerk.Core.Style
import Graphics.Craftwerk.Core.Figure

import Graphics.Craftwerk.Core.Driver.Tikz
import Graphics.Craftwerk.Core.Driver.Generic

