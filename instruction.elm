module Instruction exposing (..)

import Css exposing (padding, border3, solid, margin, color, batch, fontFamilies, fontSize, fontWeight, px, normal, width)
import Html.Styled exposing (Attribute, Html, section, styled)
import Settings exposing (theme)

instruction_description = """
Step 1:

Step 2:
Step 3:
""" ++ """ step 2 """

instruction_description_style = 
    batch
        [ fontFamilies ["monospace"]
        , fontSize (px 16)
        , fontWeight normal
        , width (px 500)
        ]

instruction_txt : List (Attribute msg) -> List (Html msg) -> Html msg
instruction_txt =
    styled section 
          [ padding (px 20)
          , width (px 500)
          , color theme.third
          , border3 (px 5) solid theme.secondary
          , margin (px 12)
          ]
