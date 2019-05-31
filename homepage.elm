module QuantumCircuit exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Html.Attributes as Attr
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput, onMouseOver)
import Array exposing (..)
import Html.String exposing (toString)
import Html.Styled.Attributes exposing (action, method, class, type_, placeholder, name, autofocus, width, height, title)
import Round as R

-- Importing modules
import Settings exposing (..)
import Utils exposing (..)
import Intro exposing (..)
import Instruction exposing (..)
import Circuit exposing (..)
import Operators exposing (..)
-- MODEL

type alias Model = { curr_x:Int, 
                     curr_y:Int, 
                     input_x : Float, -- Float,
                     input_y : Float,
                     pic_list : Array String, 
                     page : Int,
                     onselected : Int, 
                     checked : Bool,
                     ans : List Float,
                     debug : String
                   }

initModel = { curr_x = -1, 
              curr_y = -1, 
              input_x = 0, 
              input_y = 0, 
              pic_list = Array.initialize num_grid (always "null"),
              page = 1,
              onselected = 0,
              checked = False,
              ans = [],
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
        ({model | onselected = 1, checked = False, curr_x = x, curr_y = y}, Cmd.none)
    UnSelected x y gate ->
        ({model | onselected = 0,
                  pic_list = set (calc_pos x y) gate model.pic_list}, Cmd.none)
    Checkcircuit -> 
      let 
        tupe_lst = convert_pairs model.pic_list
        flag = verify2D tupe_lst
      in
        ({model | onselected = 0, checked = flag}, Cmd.none)
    Runcircuit -> 
        let 
            arr = convert_pairs model.pic_list
            mug = Debug.toString arr
        in 
            ({model | onselected = 2, ans = measure2Q (apply2Q (model.input_x, model.input_y) arr), debug = mug}, Cmd.none)
    UpdateField s fd -> 
      let 
        xs = parseFloat s
      in 
        case fd of 
          "x" -> ({model | input_x = xs, onselected = 0}, Cmd.none)
          "y" -> ({model | input_y = xs, onselected = 0}, Cmd.none)
          _ -> Debug.todo "bad input"
    Debug s -> ({model | debug = s}, Cmd.none)
    -- _ -> (model, Cmd.none)


create_img str x y curr_pic = 
      img
        [ src str
          , css (pic_style (if (x,y) == curr_pic then theme.secondary else rgb 255 255 255))
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

create_box file = 
      img
        [ src file
          , width 100
          , height 100
          , css (select_pic_style)
          ]
        []

create_cat file = 
      img
        [ src file
          , width 600
          , height 300
          --, css (select_pic_style)
          ]
        []

decodeButtonText : Decode.Decoder String
decodeButtonText =
  Decode.at ["target", "innerText" ] Decode.string

submitForm1 = frm [css [position fixed, top (vh 17), left (vw 39), fontFamilies ["monospace"]]]
                [ label []
                    [ text "1st qubit |0> state a1: "
                        , input [ type_ "text", placeholder "Float between 0 and 1", name "val_1", onInput (\str -> UpdateField str "x") ] []
                    ]
                ]

submitForm2 = frm [css [position fixed, top (vh 20), left (vw 39), fontFamilies ["monospace"]]] 
                [ label []
                    [ text "2nd qubit |0> state a2: "
                      , input [ type_ "input_val_2", placeholder "Float between 0 and 1", name "val_2", onInput (\str -> UpdateField str "y")] []
                    ]
                ]

view : Model -> Html Msg
view model =
    let styles =
          [ ("position", "fixed")
          , ("top", "50%")
          , ("left", "50%")
          , ("transform", "translate(-50%, -50%)")
          , ("Color", "green")
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

        img_cat = create_cat "cat.jpg"
        imgx_select       = create_slt "xgate.jpg" "x" curr_pic
        imgy_select       = create_slt "ygate.jpg" "y" curr_pic
        imgz_select       = create_slt "zgate.jpg" "z" curr_pic
        imgh_select       = create_slt "hgate.jpg" "h" curr_pic
        imgc_select       = create_slt "cgate.jpg" "c" curr_pic
        imgnull_select    = create_slt "test.jpg"  "null" curr_pic
        imgmeasure_select = create_slt "measure.jpg" "m" curr_pic
        img_in            = create_box "input1.jpg"
        img_in2           = create_box "input2.jpg"
        img_out           = create_box "measure.jpg"
        img_out2          = create_box "measure.jpg"
        img_uu            = create_box "result1.jpg"
        img_du            = create_box "result2.jpg"
        img_ud            = create_box "result3.jpg"
        img_dd            = create_box "result4.jpg"

        (val1, val2) = 
            case model.ans of 
                x::y::_ -> (R.round 4 x, R.round 4 y)
                _ -> ("", "")

        (val3, val4) = 
            case model.ans of 
                _::_::x::y::_ -> (R.round 4 x, R.round 4 y)
                _ -> ("", "")

        background =   div [ css 
                              [ backgroundColor theme.secondary
                              , Css.height (vh 100)
                              ] 
                            ] 
                            []
    in
    let debug_message = [text (Debug.toString model.ans ++ Debug.toString model.debug )]
        debug_message_1 = [text (List.foldr (\a -> \b -> a ++ " " ++ b) "" <| toList(model.pic_list))]
    in
    let 
        header = 
          [
            btn [ css [position fixed, top (vh 20), left (vw 12)], onClick Page1 ] [ text "Homepage" ],
            btn [ css [position fixed, top (vh 35), left (vw 12)], onClick Page2 ] [ text "Design your circuit" ],
            btn [ css [position fixed, top (vh 50), left (vw 12)], onClick Page3 ] [ text "How to run circuit" ]
          ] 
        display = 
          case model.page of 
            1 -> [ nav [css [position fixed, top (vh 7), left (vw 30)]] [img_cat] 
                ] 
                 ++ List.map (\((x,y), s) -> 
                    homepage_txt [css [homepage_description_style, position fixed, top (vh x), left (vw y)]] [text s]
                    ) homepage_description
            2 -> [
                    submitForm1,
                    submitForm2,
                    nav [css [position fixed, top (vh 28), left (vw 30)]] [img_in, img00, img01, img02, img03, img04, img_out],
                    nav [css [position fixed, top (vh 44), left (vw 30)]] [img_in2, img10, img11, img12, img13, img14, img_out2]
                    ] ++ 
                    case model.onselected of
                      1 -> [
                                nav [css [position fixed, top (vh 63), left (vw 30)]] [imgx_select, imgz_select, imgnull_select, imgh_select, imgc_select]
                            ]
                      0 -> [
                                btn [ onClick Escape,       css [position fixed, top (vh 63), left (vw 30)]] [ text "Click to reset grid" ],
                                btn [ onClick Checkcircuit, css [position fixed, top (vh 63), left (vw 42)]] [ text "Click to check circuit"],
                                btn [ onClick Runcircuit,   css [position fixed, top (vh 63), left (vw 54)]] [ text "Click to run circuit"]
                            ]
                      2 -> [                    
                                nav [css [position fixed, top (vh 63), left (vw 43), fontFamilies ["monospace"]]] [img_uu],
                                nav [css [position fixed, top (vh 63), left (vw 65), fontFamilies ["monospace"]]] [img_ud],
                                nav [css [position fixed, top (vh 78), left (vw 43), fontFamilies ["monospace"]]] [img_du],
                                nav [css [position fixed, top (vh 78), left (vw 65), fontFamilies ["monospace"]]] [img_dd],
                                div [css [position fixed, top (vh 65), left (vw 28), fontFamilies ["monospace"]]] [text "Probability of state |00>"],
                                div [css [position fixed, top (vh 65), left (vw 50), fontFamilies ["monospace"]]] [text "Probability of state |01>"],
                                div [css [position fixed, top (vh 80), left (vw 28), fontFamilies ["monospace"]]] [text "Probability of state |10>"],
                                div [css [position fixed, top (vh 80), left (vw 50), fontFamilies ["monospace"]]] [text "Probability of state |11>"],
                                div [css [position fixed, top (vh 67), left (vw 28), fontSize (px 16), fontFamilies ["monospace"]]] [text val1],                    
                                div [css [position fixed, top (vh 67), left (vw 50), fontSize (px 16), fontFamilies ["monospace"]]] [text val2],  
                                div [css [position fixed, top (vh 82), left (vw 28), fontSize (px 16), fontFamilies ["monospace"]]] [text val3],  
                                div [css [position fixed, top (vh 82), left (vw 50), fontSize (px 16), fontFamilies ["monospace"]]] [text val4]
                            ]
                      _ -> Debug.todo "impossible"
            3 -> [
                    instruction_txt [css [instruction_description_style]] [text instruction_description]
                  ]
            _ -> Debug.todo "page can only be 1,2,3"
    in 
    nav []
      ( [background] ++ header ++ display )


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