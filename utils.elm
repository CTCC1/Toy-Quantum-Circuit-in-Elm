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