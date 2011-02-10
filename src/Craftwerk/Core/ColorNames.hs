module Craftwerk.Core.ColorNames where
       
import Craftwerk.Core.Color

integralColor i = (concat (repeat stdColors)) !! fromIntegral i

white = makeColorFromIntegral 100 100 100
silver = makeColorFromIntegral 75 75 75
gray = makeColorFromIntegral 50 50 50
black = makeColorFromIntegral 0 0 0
red = makeColorFromIntegral 100 0 0
maroon = makeColorFromIntegral 50 0 0
yellow = makeColorFromIntegral 100 100 0
olive = makeColorFromIntegral 50 50 0
lime = makeColorFromIntegral 0 100 0
green = makeColorFromIntegral 0 50 0
aqua = makeColorFromIntegral 0 100 100
teal = makeColorFromIntegral 0 50 50
blue = makeColorFromIntegral 0 0 100
navy = makeColorFromIntegral 0 0 50
fuchsia = makeColorFromIntegral 100 0 100
purple = makeColorFromIntegral 50 0 50

stdColors = [ white
            , silver
            , gray 
            , black
            , red
            , maroon 
            , yellow 
            , olive
            , lime 
            , green
            , aqua 
            , teal 
            , blue 
            , navy
            , fuchsia
            , purple] 