{-# LANGUAGE TemplateHaskell #-}
module Craftwerk.Core.Test (main) where

import Test.Framework.TH
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import qualified Craftwerk.Core.Color as Color
import Craftwerk.Core.Style

import Data.Maybe

main = $(defaultMainGenerator)

prop_cap_pos x = Color.cap x >= 0
prop_cap_lim x = Color.cap x <= 1

case_style_get_lineWidth =
  getProperty emptyStyle lineWidth @=? fromJust (lineWidth defaultStyle)

case_style_get_lineColor =
  getProperty emptyStyle lineColor @=? fromJust (lineColor defaultStyle)

case_style_get_fillColor =
  getProperty emptyStyle fillColor @=? fromJust (fillColor defaultStyle)

case_style_get_stroke =
  getProperty emptyStyle stroke @=? fromJust (stroke defaultStyle)

case_style_get_fill =
  getProperty emptyStyle fill @=? fromJust (fill defaultStyle)

case_style_get_closePath =
  getProperty emptyStyle closePath @=? fromJust (closePath defaultStyle)

