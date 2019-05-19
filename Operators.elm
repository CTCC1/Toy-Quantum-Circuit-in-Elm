module Operators exposing (..)

type Result1D = Qubit  { up :Float, down : Float} | Prob1D { up :Float, down : Float }

{-
type alias Prob3D = 
  { up_up_up :Float
  , up_up_up :Float
  , up_up_up :Float
  , up_up_up :Float
  , up_up_up :Float
  , up_up_up :Float
  , up_up_up :Float
  , up_up_up :Float
  }
-}
---Single Qubit Gates---

xgate : Result1D -> Result1D
xgate input =
  case input of 
    Qubit q -> Qubit {up = q.down, down = q.up}
    _ -> input

zgate : Result1D -> Result1D
zgate input =
  case input of
    Qubit q -> Qubit {up = q.up, down = -q.down}
    _ -> input

hadamard : Result1D -> Result1D
hadamard input = 
  case input of 
    Qubit q -> Qubit { up = (q.up + q.down) / sqrt(2), down = (q.up - q.down) / sqrt(2)}
    _ -> input 

---2-Qubit Gates---

type Result2D = Qubit2D { up1 :Float, down1 : Float , up2 : Float, down2 : Float} | Prob2D { up_up :Float, up_down : Float , down_up : Float, down_down : Float} | Output2D { up1 :Float, down1 : Float , up2 : Float, down2 : Float}

---standard bipartite spin-1/2 basis: {00,01,10,11}---

{- easy stuff, just style?
id1x2 : Result2D -> Result2D
id1x2 input =

id2x1

id1z2

id2z1

id1h2

id2h1

x1x2

z1z2
......

-}
c1not2 : Result2D -> Result2D
c1not2 input =
  case input of
    Qubit2D qs -> Prob2D { up_up = qs.up1 * qs.up2, up_down = qs.up1 * qs.down2, down_up = qs.down1 * qs.down2, down_down = qs.down1 * qs.up2}
    Prob2D ps -> Prob2D { up_up = ps.up_down, up_down = ps.up_up, down_up = ps.down_up, down_down = ps.down_down}
    _ -> input

{-
c2not1 : Result2D -> Result2D
c2not1 input =
  case input of
    Qubit2D qs -> 

c1z2 : Qubit -> Qubit -> Prob2D
c1z2 q1 q2 =
  { up_up = q1.up * q2.up
  , up_down = q1.up * q2.down
  , down_up = q1.down * q2.up
  , down_down = -q1.down * q2.down
  }

c2z1  : Qubit -> Qubit -> Prob2D
c2z1 q1 q2 = c1z2 q2 q1
-}

---Measurement---

measure1Q : Result1D -> Result1D
measure1Q input =
  case input of 
    Qubit q ->
      let
          norm = q.up * q.up + q.down * q.down
      in
        Prob1D {up = q.up * q.up / norm, down = q.down * q.down / norm}
    _ -> input

--measure2Q

measure2Q : Result2D -> Result2D
measure2Q input =
  case input of
    Qubit2D qs ->
      let
        norm1 = qs.up1 ^ 2 + qs.down1 ^ 2
        norm2 = qs.up2 ^ 2 + qs.down2 ^ 2
      in
        Output2D {up1 = qs.up1 / norm1, down1 = qs.down1 / norm1, up2 = qs.up2 / norm2, down2 = qs.down2 / norm2}
    Prob2D ps ->
      let
        norm = ps.up_up ^ 2 + ps.up_down ^ 2 + ps.down_up + ps.down_down ^ 2
      in
        Output2D {up1 = (ps.up_up + ps.up_down) / norm, down1 = (ps.down_up + ps.down_down) / norm, up2 = (ps.up_up + ps.down_up) / norm, down2 = (ps.up_down + ps.down_down) / norm}
    _ -> input

---Circuit Runtime---
apply1Q : Result1D -> List (Result1D -> Result1D) -> Result1D
apply1Q init funcs =
  case funcs of
    [] -> init
    func::rest -> apply1Q (func init) rest

apply2Q : Result2D -> List (Result2D -> Result2D) -> Result2D
apply2Q init funcs =
  case funcs of
    [] -> init
    func::rest -> apply2Q (func init) rest


{- 
apply1Q : Qubit -> List String -> Qubit
apply1Q init funcs =
  case funcs of
    []      -> init
    x::rest -> 
      case x of
        "id"       -> apply1Q init rest
        "xgate"    -> apply1Q (xgate init) rest
        "zgate"    -> apply1Q (zgate init) rest
        "hadamard" -> apply1Q (hadamard init) rest
        "measure"  -> init
        _          -> Debug.todo "Invalid operators in apply1Q"
       
funcToGate2D : List (a,b) -> List String
funcToGate2D funcs =
  case funcs of
    [] -> []
    (x,y) :: rest ->
      case (x,y) of
        _ -> Debug.todo "Invalid operators in apply1Q"

apply2Q : Prob2D -> List String -> Prob2D
apply2Q init funcs =
  case funcs of
    [] -> init
    x::rest ->
      case x of
        "id" -> apply2Q init rest
        "xgate" -> apply2Q (xgate init) rest
        "zgate" -> apply2Q (zgate init) rest
        "hadamard" -> apply2Q (hadamard init) rest
        "measure" -> init
        _ -> Debug.todo "Invalid operators in apply2Q"
-} 


{-
measurement3Q : 

apply1Q

apply2

apply3 : Prob2D -> (Qubit -> Qubit) -> Prob2D
apply (a, b, c, d) (func1 func2) = 
  func (1, 0)
-}
