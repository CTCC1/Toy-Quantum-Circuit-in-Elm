module Utils exposing (..)

import Settings exposing (..)
import Array exposing (..)
import Round as R

-- helper functions -- circuit
to_pic_filename str x = case str of 
  "null"  -> "./pics/test.jpg"
  "x"     -> "./pics/xgate.jpg"
  "y"     -> "./pics/ygate.jpg"
  "z"     -> "./pics/zgate.jpg"
  "h"     -> "./pics/hgate.jpg"
  "cd"    -> "./pics/cgate.jpg"
  "cu"    -> "./pics/cgate-rev.jpg"
  "c"     -> if (x == 0) then "./pics/cgate.jpg" else "./pics/cgate-rev.jpg"
  "m"     -> "./pics/measure.jpg"
  "bridge" -> "./pics/bridge.jpg"
  _ -> Debug.todo "pic not existed!"

calc_pos x y = x * grid_width + y

my_get x y arr = case get (calc_pos x y) arr of 
  Just addr -> to_pic_filename addr x 
  Nothing -> Debug.todo "error get number not in arr"

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

ifc x = (x == "cu" || x == "cd")
isgate x = (x /= "bridge") && (x /= "c") && (ifc x == False) && (x /= "null")
verify_rotate (x,y,z) = (ifc x && ifc y) || ((ifc x) && (y == "null") && (z == "null"))

verify3D : Array (String, String, String) -> Bool
verify3D ls = 
  let 
    invalid (x,y,z) = x == "bridge" || z == "bridge" || 
                      x == "cu" || z == "cd" || 
                      verify_rotate (x,y,z) || verify_rotate (x,z,y) || 
                      verify_rotate (y,z,x) || verify_rotate (y,x,z) ||
                      verify_rotate (z,y,x) || verify_rotate (z,x,y) ||
                      (y == "bridge" && (ifc x == True) && (ifc z == True)) || 
                      (y == "bridge" && (isgate x == False) && (isgate z == False))
  in 
    isEmpty (filter invalid ls)

parseFloat : String -> Float
parseFloat string =
  case String.toFloat string of
    Just value -> value
    Nothing -> -1

take12 l =  
  case l of 
        x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
        _ -> ("", "")

take34 l = 
    case l of 
        _::_::x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
        _ -> ("", "")

take56 l =
    case l of 
        _::_::_::_::x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
        _ -> ("", "")

take78 l =
  case l of 
        _::_::_::_::_::_::x::y::_ -> (R.round 4 (x^2), R.round 4 (y^2))
        _ -> ("", "")
