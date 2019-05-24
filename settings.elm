module Settings exposing (..)

import Css exposing (Color, rgb)

grid_width  = 5 
grid_height = 2
num_grid = grid_width * grid_height

gate_lst = ["X", "Y", "Z", "H"]
num_gate = List.length gate_lst

theme : { secondary : Color, primary : Color, third : Color }
theme =
    { primary   = rgb 85 175 106
    , secondary = rgb 249 199 0 
    , third     = rgb 249 141 0 
    }
