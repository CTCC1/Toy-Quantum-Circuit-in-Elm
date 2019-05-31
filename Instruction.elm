module Instruction exposing (..)

import Css exposing (padding, border3, solid, margin, color, batch, fontFamilies, fontSize, fontWeight, px, normal, width)
import Html.Styled exposing (Attribute, Html, section, styled)
import Settings exposing (theme)


instruction_description = [ ((40+5, 26, theme.step), "-- STEP 1:")
                          , ((44+5, 26, theme.gray), "Click left tab to select whether you would like 2D \\ 3D version of your quantum circuit")
                          , ((51+5, 26, theme.step), "-- STEP 2:")
                          , ((53+5, 26, theme.gray), "Design your circuit by clicking locations and selecting gates")
                          , ((59+5, 26, theme.step), "-- STEP 3:")
                          , ((61+5, 26, theme.gray), "Input initial state a1, a2(and a3), the coefficient of 0 state (whose square is the probability of down spin) for each Qubit.")
                          , ((67+5, 26, theme.step), "-- STEP 4:")
                          , ((69+5, 26, theme.gray), "Is your circuit correctly designed? Check by ensuring all elements in your checklist goes green!")  
                          , ((77+5, 26, theme.step), "FINAL STEP:")
                          , ((79+5, 26, theme.gray), "Click run to get your answer for your quantum circuit!!")                        
                        ]
instruction_description_style = 
    batch
        [ fontFamilies ["monospace"]
        , fontSize (px 16)
        , fontWeight normal
        , width (px 600)
        ]

instruction_txt clr =
    styled section 
          [ padding (px 20)
          , width (px 600)
          , color clr
          --, border3 (px 5) solid theme.secondary
          , margin (px 12)
          ]
