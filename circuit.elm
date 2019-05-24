module Circuit exposing (..)

import Browser
import Browser.Events
import Json.Decode as Decode
import Html.Attributes as Attr
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Array exposing (..)
import Settings exposing (..)

q =3