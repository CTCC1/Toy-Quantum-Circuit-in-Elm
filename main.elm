module CountMouseClicks exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
--import Html exposing (Html)
import Html.Attributes as Attr
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Array exposing (..)

-- SOME CONSTANTS 
grid_width  = 5 
grid_height = 3
num_grid = grid_width * grid_height
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

theme : { secondary : Color, primary : Color, third : Color }
theme =
    { primary   = rgb 85 175 106
    , secondary = rgb 249 199 0 
    , third     = rgb 249 141 0 
    }


-- Helper functions
calc_pos x y = x * grid_width + y

my_get x y arr = case get (calc_pos x y) arr of 
  Just addr -> addr
  Nothing -> Debug.todo "error get number not in arr"

-- MODEL

type alias Model = { count: Int, curr_x:Int, curr_y:Int, pic_list : Array String }

initModel = { count = 0, curr_x = -1, curr_y = -1, pic_list = Array.initialize num_grid (always "test.jpg")}


-- UPDATE

type Msg = Noop | Reset | Up | Down | Left | Right | Enter | Xgate | Ygate | Zgate | Hgate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initModel, Cmd.none)
    Enter -> ({model | curr_x = -1, curr_y = -1}, Cmd.none)
    Up -> let 
              (new_x, new_y) = (model.curr_x, model.curr_y)
              (new_x_bd, new_y_bd) = if (new_x, new_y) == (-1, -1) then 
                                  (0,0)
                                else 
                                  (max 0 (new_x-1), new_y)
          in 
              ({model | curr_x = new_x_bd, curr_y = new_y_bd}, Cmd.none)
    Down -> let 
              (new_x, new_y) = (model.curr_x, model.curr_y)
              (new_x_bd, new_y_bd) = if (new_x, new_y) == (-1, -1) then 
                                  (0,0)
                                else 
                                  (min (grid_height-1) (new_x+1), new_y)
          in 
              ({model | curr_x = new_x_bd, curr_y = new_y_bd}, Cmd.none)    
    Left -> let 
              (new_x, new_y) = (model.curr_x, model.curr_y)
              (new_x_bd, new_y_bd) = if (new_x, new_y) == (-1, -1) then 
                                  (0,0)
                                else 
                                  (new_x, max 0 (new_y-1))
          in 
              ({model | curr_x = new_x_bd, curr_y = new_y_bd}, Cmd.none)
    Right -> let 
              (new_x, new_y) = (model.curr_x, model.curr_y)
              (new_x_bd, new_y_bd) = if (new_x, new_y) == (-1, -1) then 
                                  (0,0)
                                else 
                                  (new_x, min (grid_width-1) (new_y+1))
          in 
              ({model | curr_x = new_x_bd, curr_y = new_y_bd}, Cmd.none)
    Xgate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "xgate.jpg" model.pic_list}, Cmd.none)
    Ygate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "Ygate.jpg" model.pic_list}, Cmd.none)
    Zgate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "Zgate.jpg" model.pic_list}, Cmd.none)
    Hgate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "Hgate.jpg" model.pic_list}, Cmd.none)

create_img str x y curr_pic = 
      img
        [ src str
          , css
            [ padding (px 20)
              , width (px 50)
              , height (px 50)
              , border3 (px 5) solid (rgb 120 120 120)
              , borderColor (if (x,y) == curr_pic then theme.secondary else theme.third) 
              , hover
                  [ borderColor theme.primary
                  , borderRadius (px 10)
                  ]
              ]
          ]
        []

view : Model -> Html Msg
view model =
    let styles =
          [ ("position", "fixed")
          , ("top", "50%")
          , ("left", "50%")
          , ("transform", "translate(-50%, -50%)")

          ]
        curr_pic = (model.curr_x, model.curr_y)
        img00  = create_img (my_get 0 0 model.pic_list) 0 0 curr_pic
        img01  = create_img (my_get 0 1 model.pic_list) 0 1 curr_pic
        img02  = create_img (my_get 0 2 model.pic_list) 0 2 curr_pic
        img03  = create_img (my_get 0 3 model.pic_list) 0 3 curr_pic
        img04  = create_img (my_get 0 4 model.pic_list) 0 4 curr_pic
        img10  = create_img (my_get 1 0 model.pic_list) 1 0 curr_pic
        img11  = create_img (my_get 1 1 model.pic_list) 1 1 curr_pic
        img12  = create_img (my_get 1 2 model.pic_list) 1 2 curr_pic
        img13  = create_img (my_get 1 3 model.pic_list) 1 3 curr_pic
        img14  = create_img (my_get 1 4 model.pic_list) 1 4 curr_pic
        img20  = create_img (my_get 2 0 model.pic_list) 2 0 curr_pic
        img21  = create_img (my_get 2 1 model.pic_list) 2 1 curr_pic
        img22  = create_img (my_get 2 2 model.pic_list) 2 2 curr_pic
        img23  = create_img (my_get 2 3 model.pic_list) 2 3 curr_pic
        img24  = create_img (my_get 2 4 model.pic_list) 2 4 curr_pic
    in
    let display = text ("Count: " ++ Debug.toString model.curr_x ++ Debug.toString model.curr_y) 
        display2 = text (List.foldr (\a -> \b -> a ++ " " ++ b) "" <| toList(model.pic_list))
    in
    nav [css [position fixed, top (vh 30), left (vw 30)]]
      [
        nav [] [img00, img01, img02, img03, img04],
        nav [] [img10, img11, img12, img13, img14],
        nav [] [img20, img21, img22, img23, img24],
        btn [ onClick Reset ] [ text "Click me!" ]
      ]


-- SUBSCRIPTIONS

-- https://github.com/elm/browser/blob/1.0.0/notes/keyboard.md
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Browser.Events.onKeyDown
        (Decode.map (\key -> 
            case key of 
              "ArrowLeft" -> Left
              "ArrowRight" -> Right
              "ArrowUp" -> Up
              "ArrowDown" -> Down
              "Enter" -> Enter
              "x" -> Xgate
              "y" -> Ygate
              "z" -> Zgate
              "h" -> Hgate
              _ -> Noop) 
          keyDecoder)
    ]

-- MAIN

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }