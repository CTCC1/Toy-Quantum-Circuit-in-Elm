module QuantumCircuit exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Html.Attributes as Attr
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
import Array exposing (..)
import Html.String exposing (toString)
import Html.Styled.Attributes exposing (action, method, class, type_, placeholder, name, autofocus, width, height)
-- Importing modules
import Settings exposing (..)
import Utils exposing (..)
import Homepage exposing (..)
import Instruction exposing (..)
import Circuit exposing (..)
-- MODEL

type alias Model = { curr_x:Int, 
                     curr_y:Int, 
                     input_x : Float, -- Float,
                     input_y : Float,
                     pic_list : Array String, 
                     page : Int,
                     onselected : Bool,
                     checked : Bool,
                     debug : String
                   }

initModel = { curr_x = -1, 
              curr_y = -1, 
              input_x = 0, 
              input_y = 0, 
              pic_list = Array.initialize num_grid (always "null"),
              page = 1,
              onselected = False,
              checked = False,
              debug = ""
            }

-- UPDATE

type Msg = Noop | Reset | Escape | 
            Selected Int Int | UnSelected Int Int String | 
            Checkcircuit | Runcircuit  | Debug String |
            Page1 | Page2 | Page3 |
            UpdateField String String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initModel, Cmd.none)
    Escape -> ({model | curr_x = -1,
                        curr_y = -1, 
                        pic_list = Array.initialize num_grid (always "null")
                }, Cmd.none)
    Page1 -> ({model | page = 1}, Cmd.none)
    Page2 -> ({model | page = 2}, Cmd.none)
    Page3 -> ({model | page = 3}, Cmd.none)
    Selected x y -> 
        ({model | onselected = True, checked = False, curr_x = x, curr_y = y}, Cmd.none)
    UnSelected x y gate ->
        ({model | onselected = False,
                  pic_list = set (calc_pos x y) gate model.pic_list}, Cmd.none)
    Checkcircuit -> 
      let 
        tupe_lst = convert_pairs model.pic_list
        flag = verify2D tupe_lst
      in
        ({model | checked = flag}, Cmd.none)
    Runcircuit -> Debug.todo "todo"
    UpdateField s fd -> 
      let 
        xs = parseFloat s
      in 
        case fd of 
          "x" -> ({model | input_x = xs}, Cmd.none)
          "y" -> ({model | input_y = xs}, Cmd.none)
          _ -> Debug.todo "bad input"
    Debug s -> ({model | debug = s}, Cmd.none)
    -- _ -> (model, Cmd.none)


create_img str x y curr_pic = 
      img
        [ src str
          , css (pic_style (if (x,y) == curr_pic then theme.secondary else theme.third))
          , onClick (Selected x y)
          , onDoubleClick (Noop)
          ]
        []

create_slt name gate (x, y) = 
      img
        [ src name
          , width 30
          , height 30
          , css select_pic_style
          , onClick (UnSelected x y gate)
          ]
        []

decodeButtonText : Decode.Decoder String
decodeButtonText =
  Decode.at ["target", "innerText" ] Decode.string

viewForm : Model -> Html Msg
viewForm model =
    frm []
        [ label []
            [ text "Enter value for y"
            , input [ type_ "text", 
                      placeholder "Enter value for x", 
                      name "Enter value for x", 
                      autofocus True,
                      onInput (\str -> UpdateField str "x") 
                    ]
              []
            ]
        , label []
            [ text "Enter value for y"
            , input [ type_ "input_val_2", 
                      placeholder "Enter value for y", 
                      name "Enter value for y",
                      autofocus True,
                      onInput (\str -> UpdateField str "y")
                    ] 
              []
            ]
        ]

submitForm1 = frm []
                [ label []
                  [ text "input_val_1"
                    , input [ type_ "text", placeholder "val_1", name "val_1", onInput (\str -> UpdateField str "x") ] []
                    ]
                ]

submitForm2 = frm [] 
                [ label []
                    [ text "input_val_2"
                      , input [ type_ "input_val_2", placeholder "val_2", name "val_2", onInput (\str -> UpdateField str "y")] []
                    ]
                ]

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

        imgx_select       = create_slt "xgate.jpg" "x" curr_pic
        imgy_select       = create_slt "ygate.jpg" "y" curr_pic
        imgz_select       = create_slt "zgate.jpg" "z" curr_pic
        imgh_select       = create_slt "hgate.jpg" "h" curr_pic
        imgc_select       = create_slt "cgate.jpg" "c" curr_pic
        imgnull_select    = create_slt "test.jpg"  "null" curr_pic
        imgmeasure_select = create_slt "measure.jpg" "m" curr_pic
        -- img_out = create_outbox
    in
    let debug_message = [text (Debug.toString model.input_x ++ Debug.toString model.input_y)]
        debug_message_1 = [text (List.foldr (\a -> \b -> a ++ " " ++ b) "" <| toList(model.pic_list))]
    in
    let 
        header = 
          [
            btn [ onClick Page1 ] [ text "Homepage" ],
            btn [ onClick Page2 ] [ text "Design your circuit" ],
            btn [ onClick Page3 ] [ text "How to run circuit" ]
          ] 
        display = 
          case model.page of 
            1 -> [
                    homepage_txt [css [homepage_description_style]] [text homepage_description]
                  ]
            2 -> [
                    submitForm1,
                    submitForm2,
                    btn [onClick Noop] [text "Click to submit your input"],
                    nav [] [img00, img01, img02, img03, img04],
                    nav [] [img10, img11, img12, img13, img14]
                    ] ++ 
                    if model.onselected == True then 
                      [
                        nav [] [imgx_select, imgz_select, imgnull_select],
                        nav [] [imgh_select, imgc_select, imgmeasure_select]
                      ]
                    else 
                      [
                        btn [ onClick Escape ] [ text "Click to reset grid" ],
                        btn [ onClick Checkcircuit ] [ text "Click to check circuit"],
                        btn [ onClick Runcircuit ] [ text "Click to run circuit"]
                      ]
            3 -> [
                    instruction_txt [css [instruction_description_style]] [text instruction_description]
                  ]
            _ -> Debug.todo "page can only be 1,2,3"
    in 
    nav [css [position fixed, top (vh 10), left (vw 30)]]
      (debug_message_1 ++ header ++ display )


-- SUBSCRIPTIONS

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
              "Escape" -> Escape
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