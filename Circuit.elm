module Circuit exposing (..)

import Css exposing (margin, fontSize, color, backgroundColor, textDecoration, underline, solid, padding, width, height, hover, borderColor, px, borderRadius, rgb, border3)
import Settings exposing (theme, grid_width)
import Html.Styled.Attributes exposing (action, method, class, type_, placeholder, name, title)
import Html.Styled exposing (Html, Attribute, label, input, text, button, form, styled)
import Array.Extra as Array

urlMirrorService : String
urlMirrorService =
    "https://httpbin.org/post"

select_pic_style = 
  [ width (px 90)
    , height (px 90)
    , border3 (px 5) solid (rgb 120 120 120)
    , borderColor (rgb 255 255 255)
    ]


pic_style clr = 
  [ width (px 90)
  , height (px 90)
  , border3 (px 5) solid (rgb 120 120 120)
  , borderColor clr
  , hover
      [ borderColor theme.primary
      , borderRadius (px 10)
      ]
  ]

convert_pairs a = 
  let 
    left   = Array.sliceUntil grid_width a 
    right  = Array.sliceUntil grid_width (Array.sliceFrom grid_width a)
  in 
    Array.zip left right

convert_tri a = 
  let 
    left    = Array.sliceUntil grid_width a 
    right   = Array.sliceFrom grid_width (Array.sliceFrom grid_width a)
    middle  = Array.sliceUntil grid_width (Array.sliceFrom grid_width a)
  in 
    Array.zip3 left middle right

frm : List (Attribute msg) -> List (Html msg) -> Html msg
frm =
    styled form
        [ margin (px 5)
        , fontSize (px 12)
        , color theme.third
        , hover
            [ backgroundColor theme.secondary
            , textDecoration underline
            ]
        ]




