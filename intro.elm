module Intro exposing (..)

import Css exposing (padding, border3, solid, margin, color, batch, fontFamilies, fontSize, fontWeight, px, normal, width, underline, hover, textDecoration, backgroundColor)
import Html.Styled exposing (Attribute, Html, section, styled, button)
import Settings exposing (theme)

homepage_description = [  ((40+5, 26), "Welcome to our Visualization of Basic Quantum Circuits! ")
                        , ((44+5, 26), "Click the buttons to design your own quantum circuits / view instructions!")
                        , ((49+5, 26), "NOTES:")
                        , ((52+5, 26), "Unlike a traditional circuit where a bit is 1 or 0, a Qubit in quantum computing can also be in a superposition of the 1 and 0 states.")
                        , ((58+5, 26), "You will be able to build a simple quantum circuit consisting of the basic quantum gates, input the initial qubit states, and then see the result!")
                        , ((65+5, 26), "In quantum mechanics, measurement make the qubits collapsed to a certain state and classical information emerged. So what we will show to you as the result of circuits are the probabilities of the outcomes depending on the quantum state they were in.")
                        ]

homepage_description_style = 
    batch
        [ fontFamilies ["monospace"]
        , fontSize (px 16)
        , fontWeight normal
        , width (px 700)
        ]

homepage_txt : List (Attribute msg) -> List (Html msg) -> Html msg
homepage_txt =
    styled section 
          [ padding (px 10)
          , width (px 800)
          , color theme.third
          --, border3 (px 5) solid theme.secondary
          , margin (px 12)
          ]

btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn =
    styled button
        [ margin (px 12)
        , fontSize (px 12)
        , color theme.third
        , hover
            [ backgroundColor theme.secondary
            , textDecoration underline
            ]
        ]