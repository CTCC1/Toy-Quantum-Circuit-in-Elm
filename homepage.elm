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
                     input_x : Float,
                     input_y : Float,
                     input_z : Float, 
                     pic_list : Array String, 
                     page : Int,
                     onselected : Int, 
                     checked : Bool,
                     ans : List Float,
                     debug : String
                   }

initModel = { curr_x = -1, 
              curr_y = -1, 
              input_x = -1, 
              input_y = -1, 
              input_z = -1,
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
            Page1 | Page2 | Page3 | Page4 |
            UpdateField String String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initModel, Cmd.none)
    Escape -> ({initModel | page = model.page}, Cmd.none)
    Page1 -> ({initModel | page = 1}, Cmd.none)
    Page2 -> ({initModel | page = 2}, Cmd.none)
    Page3 -> ({initModel | page = 3}, Cmd.none)
    Page4 -> ({initModel | page = 4}, Cmd.none)
    Selected x y -> 
        ({model | onselected = 1, checked = False, curr_x = x, curr_y = y}, Cmd.none)
    UnSelected x y gate ->
        ({model | onselected = 0,
                  pic_list = set (calc_pos x y) gate model.pic_list}, Cmd.none)
    Checkcircuit -> 
      let 
          flag = if model.page == 2 then 
                    verify2D (convert_pairs model.pic_list)
                  else 
                    verify3D (convert_tri model.pic_list)
      in
        ({model | onselected = 0, checked = flag}, Cmd.none)
    Runcircuit -> 
        if (model.checked == True) then 
          let 
            real_ans = if model.page == 2 then 
                        measure2Q (apply2Q (model.input_x, model.input_y) (convert_pairs model.pic_list))
                       else 
                        measure3Q (apply3Q (model.input_x, model.input_y, model.input_z) (convert_tri model.pic_list))
          in 
            ({model | onselected = 2, ans = real_ans, debug = Debug.toString (convert_tri model.pic_list)}, Cmd.none)
        else 
          (model, Cmd.none)
    UpdateField s fd -> 
      let 
        xs = parseFloat s
      in 
        case fd of 
          "x" -> ({model | input_x = xs, onselected = 0}, Cmd.none)
          "y" -> ({model | input_y = xs, onselected = 0}, Cmd.none)
          "z" -> ({model | input_z = xs, onselected = 0}, Cmd.none)
          _ -> Debug.todo "bad input"
    Debug s -> ({model | debug = s}, Cmd.none)
    -- _ -> (model, Cmd.none)


create_img str x y curr_pic = 
      img
        [ src str
          , css (pic_style (if (x,y) /= curr_pic then rgb 255 255 255 else theme.fourth))
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

my_font1 my_pred = [fontFamilies ["monospace"], if my_pred then color theme.green else color theme.red, fontSize (px 16)]
my_font = [fontFamilies ["monospace"], color theme.third, fontSize (px 16)]

decodeButtonText : Decode.Decoder String
decodeButtonText =
  Decode.at ["target", "innerText" ] Decode.string

submitForm1 x y = frm [css [position fixed, top (vh x), left (vw y), fontFamilies ["monospace"]]]
                [ label []
                    [ text "1st qubit |0> state a1: "
                        , input [ type_ "text", placeholder "Float between 0 and 1", name "val_1", onInput (\str -> UpdateField str "x") ] []
                    ]
                ]

submitForm2 x y = frm [css [position fixed, top (vh x), left (vw y), fontFamilies ["monospace"]]] 
                [ label []
                    [ text "2nd qubit |0> state a2: "
                      , input [ type_ "input_val_2", placeholder "Float between 0 and 1", name "val_2", onInput (\str -> UpdateField str "y")] []
                    ]
                ]

submitForm3 x y = frm [css [position fixed, top (vh x), left (vw y), fontFamilies ["monospace"]]] 
                [ label []
                    [ text "3rd qubit |0> state a3: "
                      , input [ type_ "input_val_3", placeholder "Float between 0 and 1", name "val_3", onInput (\str -> UpdateField str "z")] []
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

        img20  = create_img (my_get 2 0 model.pic_list) 2 0 curr_pic
        img21  = create_img (my_get 2 1 model.pic_list) 2 1 curr_pic
        img22  = create_img (my_get 2 2 model.pic_list) 2 2 curr_pic
        img23  = create_img (my_get 2 3 model.pic_list) 2 3 curr_pic
        img24  = create_img (my_get 2 4 model.pic_list) 2 4 curr_pic

        img_cat = create_cat "./pics/cat.jpg"
        imgx_select       = create_slt "./pics/xgate.jpg" "x" curr_pic
        imgy_select       = create_slt "./pics/ygate.jpg" "y" curr_pic
        imgz_select       = create_slt "./pics/zgate.jpg" "z" curr_pic
        imgh_select       = create_slt "./pics/hgate.jpg" "h" curr_pic
        imgc_select       = create_slt "./pics/cgate.jpg" "c" curr_pic
        imgcu_select      = create_slt "./pics/cgate-rev.jpg" "cu" curr_pic
        imgcd_select      = create_slt "./pics/cgate.jpg" "cd" curr_pic          
        imgnull_select    = create_slt "./pics/test.jpg"  "null" curr_pic
        imgmeasure_select = create_slt "./pics/measure.jpg" "m" curr_pic

        img_in            = create_box "./pics/input1.jpg"
        img_in2           = create_box "./pics/input2.jpg"
        img_in3           = create_box "./pics/input3.jpg"
        img_out           = create_box "./pics/measure.jpg"
        img_out2          = create_box "./pics/measure.jpg"
        img_out3          = create_box "./pics/measure.jpg"

        img_uu            = create_box "./pics/result1.jpg"
        img_du            = create_box "./pics/result2.jpg"
        img_ud            = create_box "./pics/result3.jpg"
        img_dd            = create_box "./pics/result4.jpg"

        (val1, val2) = 
            case model.ans of 
                x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
                _ -> ("", "")

        (val3, val4) = 
            case model.ans of 
                _::_::x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
                _ -> ("", "")

        (val5, val6) = 
            case model.ans of 
                _::_::_::_::x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
                _ -> ("", "")

        (val7, val8) = 
            case model.ans of 
                _::_::_::_::_::_::x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
                _ -> ("", "")

        background =   div [ css 
                              [ backgroundColor theme.secondary
                              , Css.height (vh 100)
                              ] 
                            ] 
                            []
    in
    let debug_message = [text (Debug.toString model.ans)]
        debug_message_1 = [text (Debug.toString (convert_tri model.pic_list))]
    in
    let 
        header = 
          [
            btn [ css [position fixed, top (vh 10), left (vw 10)], onClick Page1 ] [ text "Homepage" ],
            btn [ css [position fixed, top (vh 15), left (vw 10)], onClick Page2 ] [ text "Design your circuit - 2D" ],
            btn [ css [position fixed, top (vh 20), left (vw 10)], onClick Page3 ] [ text "Design your circuit - 3D" ],
            btn [ css [position fixed, top (vh 25), left (vw 10)], onClick Page4 ] [ text "How to run circuit" ]
          ] 
        display = 
          case model.page of 
            1 -> [ nav [css [position fixed, top (vh 7), left (vw 30)]] [img_cat] 
                ] 
                 ++ List.map (\((x,y), s) -> 
                    homepage_txt [css [homepage_description_style, position fixed, top (vh x), left (vw y)]] [text s]
                    ) homepage_description
            2 -> [
                    submitForm1 35 7,
                    submitForm2 40 7,
                    nav [css [position fixed, top (vh 14), left (vw 30)]] [img_in, img00, img01, img02, img03, img04, img_out],
                    nav [css [position fixed, top (vh 28), left (vw 30)]] [img_in2, img10, img11, img12, img13, img14, img_out2]
                    ] ++ 
                    case model.onselected of
                      1 -> [
                                nav [css [position fixed, top (vh 63), left (vw 30)]] [imgx_select, imgz_select, imgnull_select, imgh_select, imgc_select]
                            ]
                      0 -> [
                                btn [ onClick Escape,       css [position fixed, top (vh 45), left (vw 12)]] [ text "Click to reset" ],
                                btn [ onClick Checkcircuit, css [position fixed, top (vh 63), left (vw 42)]] [ text "Click to check circuit"],
                                btn [ onClick Runcircuit,   css [position fixed, top (vh 63), left (vw 54)]] [ text "Click to run circuit"],
                                nav [css ([position fixed, top (vh 61), left (vw 7)] ++ my_font1 (True))] [text "Checklist:"],
                                nav [css ([position fixed, top (vh 64), left (vw 7)] ++ my_font1 (model.input_x <= 1 && model.input_x >=0))] [text "1. Value a1 is between 0 and 1"],
                                nav [css ([position fixed, top (vh 67), left (vw 7)] ++ my_font1 (model.input_y <= 1 && model.input_y >=0))] [text "2. Value a2 is between 0 and 1"],
                                nav [css ([position fixed, top (vh 70), left (vw 7)] ++ my_font1 (model.checked == True))] [text "3. Circuit is checked before run"]
                             ]
                      2 -> [                    
                                nav [css ([position fixed, top (vh 53), left (vw 43)])] [img_uu],
                                nav [css ([position fixed, top (vh 53), left (vw 69)])] [img_ud],
                                nav [css ([position fixed, top (vh 68), left (vw 43)])] [img_du],
                                nav [css ([position fixed, top (vh 68), left (vw 69)])] [img_dd],

                                div [css ([position fixed, top (vh 55), left (vw 25)] ++ my_font)] [text "Probability of state |00>"],
                                div [css ([position fixed, top (vh 55), left (vw 52)] ++ my_font)] [text "Probability of state |01>"],
                                div [css ([position fixed, top (vh 70), left (vw 25)] ++ my_font)] [text "Probability of state |10>"],
                                div [css ([position fixed, top (vh 70), left (vw 52)] ++ my_font)] [text "Probability of state |11>"],

                                div [css ([position fixed, top (vh 57), left (vw 30)] ++ my_font)] [text val1],                    
                                div [css ([position fixed, top (vh 57), left (vw 57)] ++ my_font)] [text val2],  
                                div [css ([position fixed, top (vh 72), left (vw 30)] ++ my_font)] [text val3],  
                                div [css ([position fixed, top (vh 72), left (vw 57)] ++ my_font)] [text val4]
                            ]
                      _ -> Debug.todo "impossible"
            3 -> [
                    submitForm1 35 7,
                    submitForm2 40 7,
                    submitForm3 45 7,
                    nav [css [position fixed, top (vh 14), left (vw 30)]] [img_in, img00, img01, img02, img03, img04, img_out],
                    nav [css [position fixed, top (vh 28), left (vw 30)]] [img_in2, img10, img11, img12, img13, img14, img_out2],
                    nav [css [position fixed, top (vh 42), left (vw 30)]] [img_in3, img20, img21, img22, img23, img24, img_out3]
                    ] ++ 
                    case model.onselected of
                      1 -> [
                                nav [css [position fixed, top (vh 63), left (vw 30)]] ([imgx_select, imgz_select, imgnull_select, imgh_select] ++ (if model.page == 2 then [imgc_select] else [imgcu_select, imgcd_select]))
                            ]
                      0 -> [
                                btn [ onClick Escape,       css [position fixed, top (vh 50), left (vw 12)]] [ text "Click to reset" ],
                                btn [ onClick Checkcircuit, css [position fixed, top (vh 63), left (vw 42)]] [ text "Click to check circuit"],
                                btn [ onClick Runcircuit,   css [position fixed, top (vh 63), left (vw 54)]] [ text "Click to run circuit"],
                                nav [css ([position fixed, top (vh 61), left (vw 7)] ++ my_font1 (True))] [text "Checklist:"],
                                nav [css ([position fixed, top (vh 64), left (vw 7)] ++ my_font1 (model.input_x <= 1 && model.input_x >=0))] [text "1. Value a1 is between 0 and 1"],
                                nav [css ([position fixed, top (vh 67), left (vw 7)] ++ my_font1 (model.input_y <= 1 && model.input_y >=0))] [text "2. Value a2 is between 0 and 1"],
                                nav [css ([position fixed, top (vh 70), left (vw 7)] ++ my_font1 (model.input_z <= 1 && model.input_z >=0))] [text "3. Value a3 is between 0 and 1"],
                                nav [css ([position fixed, top (vh 73), left (vw 7)] ++ my_font1 (model.checked == True))] [text "4. Circuit is checked before run"]
                             ]
                      2 -> [                    
                                nav [css ([position fixed, top (vh 63), left (vw 43)])] [img_uu],
                                nav [css ([position fixed, top (vh 63), left (vw 65)])] [img_ud],
                                nav [css ([position fixed, top (vh 78), left (vw 43)])] [img_du],
                                nav [css ([position fixed, top (vh 78), left (vw 65)])] [img_dd],
                                div [css ([position fixed, top (vh 65), left (vw 28)] ++ my_font)] [text "Probability of state |000>"],
                                div [css ([position fixed, top (vh 65), left (vw 50)] ++ my_font)] [text "Probability of state |001>"],
                                div [css ([position fixed, top (vh 80), left (vw 28)] ++ my_font)] [text "Probability of state |010>"],
                                div [css ([position fixed, top (vh 80), left (vw 50)] ++ my_font)] [text "Probability of state |011>"],
                                div [css ([position fixed, top (vh 65), left (vw 28)] ++ my_font)] [text "Probability of state |100>"],
                                div [css ([position fixed, top (vh 65), left (vw 50)] ++ my_font)] [text "Probability of state |101>"],
                                div [css ([position fixed, top (vh 80), left (vw 28)] ++ my_font)] [text "Probability of state |110>"],
                                div [css ([position fixed, top (vh 80), left (vw 50)] ++ my_font)] [text "Probability of state |111>"],

                                div [css ([position fixed, top (vh 67), left (vw 28)] ++ my_font)] [text val1],                    
                                div [css ([position fixed, top (vh 67), left (vw 50)] ++ my_font)] [text val2],  
                                div [css ([position fixed, top (vh 82), left (vw 28)] ++ my_font)] [text val3],  
                                div [css ([position fixed, top (vh 82), left (vw 50)] ++ my_font)] [text val4],
                                div [css ([position fixed, top (vh 87), left (vw 28)] ++ my_font)] [text val5],                    
                                div [css ([position fixed, top (vh 87), left (vw 50)] ++ my_font)] [text val6],  
                                div [css ([position fixed, top (vh 92), left (vw 28)] ++ my_font)] [text val7],  
                                div [css ([position fixed, top (vh 92), left (vw 50)] ++ my_font)] [text val8]
                            ]
                      _ -> Debug.todo "impossible"
            4 -> [
                    instruction_txt [css [instruction_description_style]] [text instruction_description]
                  ]
            _ -> Debug.todo "page can only be 1,2,3"
    in 
    nav []
      (  [background]++ header ++ display )


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