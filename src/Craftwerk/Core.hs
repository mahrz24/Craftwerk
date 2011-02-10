
-- |
-- Module      :  Craftwerk.Core
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- A high-level and easy to use graphics library with several backends, 
-- currently including Cairo(+Gtk UI), Gloss and TikZ.
--
-- Thanks to Ben Lippmeier for inspirations from the gloss library
--

module Craftwerk.Core 
       (
         module Craftwerk.Core.Color
       , module Craftwerk.Core.ColorNames
       , module Craftwerk.Core.Style
       , module Craftwerk.Core.Figure

       , module Craftwerk.Core.Driver.Cairo
       , module Craftwerk.Core.Driver.Gloss
       , module Craftwerk.Core.Driver.Tikz 
       ) where 

import Craftwerk.Core.Color
import Craftwerk.Core.ColorNames
import Craftwerk.Core.Style
import Craftwerk.Core.Figure

import Craftwerk.Core.Driver.Cairo
import Craftwerk.Core.Driver.Gloss
import Craftwerk.Core.Driver.Tikz