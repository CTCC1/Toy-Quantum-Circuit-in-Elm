module Settings exposing (..)

import Css exposing (Color, rgb)

grid_width  = 5 
grid_height = 2
num_grid = grid_width * grid_height

gate_lst = ["X", "Y", "Z", "H"]
num_gate = List.length gate_lst

theme : { secondary : Color, primary : Color, third : Color }
theme =
    { primary   = rgb 23 63 95
    , secondary = rgb 245 245 220
    , third     = rgb 32 99 155
    }
