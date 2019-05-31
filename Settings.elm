module Settings exposing (..)

import Css exposing (Color, rgb)

grid_width  = 5 
grid_height = 3
num_grid = grid_width * grid_height

gate_lst = ["X", "Y", "Z", "H", "CU", "CD", "BRIDGE"]
num_gate = List.length gate_lst

theme : { secondary : Color, primary : Color, third : Color, fourth : Color, red : Color, green : Color , step : Color, gray : Color}
theme =
    { primary   = rgb 225 198 15
    , secondary = rgb 245 245 220
    , third     = rgb 32 99 155
    , fourth    = rgb 207 185 151
    , red       = rgb 255 8 0
    , green     = rgb 94 119 3
    , step      = rgb 213 117 0 
    , gray      = rgb 90 39 41
    }
