module QuantumCircuit exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Html.Attributes as Attr
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Array exposing (..)

-- Importing modules
import Settings exposing (..)
import Utils exposing (..)
import Homepage exposing (..)
import Instruction exposing (..)
import Circuit exposing (..)
-- MODEL

type alias Model = { curr_x:Int, 
                     curr_y:Int, 
                     pic_list : Array String, 
                     page : Int,
                     onselected : Bool
                   }

initModel = { curr_x = -1, 
              curr_y = -1, 
              pic_list = Array.initialize num_grid (always "null"),
              page = 1,
              onselected = False
            }

-- UPDATE

type Msg = Noop | Reset | Escape | Selected Int Int | UnSelected Int Int String | Checkcircuit |
            Up | Down | Left | Right | 
            Xgate Int Int | Ygate Int Int | Zgate Int Int | Hgate Int Int | Cgate Int Int | Emptygate Int Int |
            Page1 | Page2 | Page3


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
        ({model | onselected = True, curr_x = x, curr_y = y}, Cmd.none)
    UnSelected x y gate ->
        ({model | onselected = False,
                  pic_list = set (calc_pos x y) gate model.pic_list}, Cmd.none)
    _ -> (model, Cmd.none)
    {-Up -> let 
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
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "x" model.pic_list}, Cmd.none)
    Ygate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "y" model.pic_list}, Cmd.none)
    Zgate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "z" model.pic_list}, Cmd.none)
    Hgate -> 
          if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "h" model.pic_list}, Cmd.none) 
    EmptyGate ->
           if (model.curr_x == -1 && model.curr_y == -1) then 
            (model, Cmd.none)
          else 
            ({model | pic_list = set (calc_pos model.curr_x model.curr_y) "null" model.pic_list}, Cmd.none) -}
         

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
          , onClick (Selected x y)
          , onDoubleClick (Noop)
          ]
        []

create_slt name gate (x, y) = 
      img
          [ src name
            , css
              [ padding (px 20)
                , width (px 50)
                , height (px 50)
                , hover
                    [ borderColor theme.primary
                    , borderRadius (px 10)
                    ]
                ]
            , onClick (UnSelected x y gate)
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
        -- img_in = create_inbox
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
        -- img_out = create_outbox
    in
    let debug_message = [text (Debug.toString model.onselected ++ Debug.toString model.curr_x ++ Debug.toString model.curr_y)]
        debug_message_1 = text (List.foldr (\a -> \b -> a ++ " " ++ b) "" <| toList(model.pic_list))
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
                    nav [] [img00, img01, img02, img03, img04],
                    nav [] [img10, img11, img12, img13, img14],
                    if model.onselected == True then 
                      nav [] [imgx_select, imgy_select, imgz_select, imgh_select, imgc_select, imgnull_select]
                    else 
                      btn [ onClick Escape ] [ text "Click to reset grid" ],
                    btn [ onClick Checkcircuit ] [ text "Click to check circuit" ]
                  ]
            3 -> [
                    instruction_txt [css [instruction_description_style]] [text instruction_description]
                  ]
            _ -> Debug.todo "page can only be 1,2,3"
    in 
    nav [css [position fixed, top (vh 10), left (vw 30)]]
      ( header ++ display )


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
              "Escape" -> Escape
              --"x" -> Xgate
              --"y" -> Ygate
              --"z" -> Zgate
              --"h" -> Hgate
              --"c" -> Cgate
              --"e" -> Emptygate
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