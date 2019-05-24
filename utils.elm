module Utils exposing (..)

import Settings exposing (..)
import Array exposing (..)

-- helper functions -- circuit
to_pic_filename str = case str of 
  "null" -> "test.jpg"
  "x" -> "xgate.jpg"
  "y" -> "ygate.jpg"
  "z" -> "zgate.jpg"
  "h" -> "hgate.jpg"
  "c" -> "cgate.jpg"
  _ -> Debug.todo "pic not existed!"

calc_pos x y = x * grid_width + y

my_get x y arr = case get (calc_pos x y) arr of 
  Just addr -> to_pic_filename(addr)
  Nothing -> Debug.todo "error get number not in arr"

menu = Debug.todo "todo"

first2d : (a1, a2) -> a1
first2d (x,_) = x

second2d : (a1, a2) -> a2
second2d (_,y) = y

verify2D : Array (String, String) -> Bool
verify2D ls =
  let
    invalid (x,y) = (x == "c" && y == "c") || (x == "c" && y == "null") || (x == "null" && y == "c")
  in
    isEmpty (filter invalid ls)
