module Homepage exposing (..)

import Css exposing (padding, border3, solid, margin, color, batch, fontFamilies, fontSize, fontWeight, px, normal, width, underline, hover, textDecoration, backgroundColor)
import Html.Styled exposing (Attribute, Html, section, styled, button)
import Settings exposing (theme)

homepage_description = """The goal of the project is to build an interactive visualization of basic quantum circuits. 
Unlike a traditional circuit where you can check whether a bit is 1 or 0, measurement in quantum mechanics make the qubit collapsed to a certain state and classical information emerged. 
So, the project is to vividly depict some quirks of the quantum circuits targeting a general audience. 
It should consist of several key examples that the viewers can interact with.  

Aspects of the project that will build on Elm experience already developed:  
An MVC model for a basic interactive visualization website would be used  
to handle some user input, namely the initial state of each qubit and where measurement is made.  

Aspects of the project that will require additional tools and techniques to implement:  
We will need to find some way to implement the drawing of quantum circuits using a certain graphics library, as well as implementing the logic of quantum gates and multiple interaction aspects."""

homepage_description_style = 
    batch
        [ fontFamilies ["monospace"]
        , fontSize (px 16)
        , fontWeight normal
        , width (px 500)
        ]

homepage_txt : List (Attribute msg) -> List (Html msg) -> Html msg
homepage_txt =
    styled section 
          [ padding (px 20)
          , width (px 500)
          , color theme.third
          , border3 (px 5) solid theme.secondary
          , margin (px 12)
          ]

btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn =
    styled button
        [ margin (px 12)
        , color theme.primary
        , hover
            [ backgroundColor theme.secondary
            , textDecoration underline
            ]
        ]