module Operators exposing (..)

import Array exposing (..)

type Result1D = Qubit  { up :Float, down : Float} | Prob1D { up :Float, down : Float }

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

swap2D: Result2D -> Result2D
swap2D input =
  case input of
    Qubit2D qs -> Qubit2D { up1 = qs.up2, down1 = qs.down2, up2 = qs.up1, down2 = qs.down1}
    Prob2D ps -> Prob2D { up_up = ps.up_up, up_down = ps.down_up , down_up = ps.up_down, down_down = ps.down_down} 
    _ -> input

id1id2 : Result2D -> Result2D
id1id2 input = input

id1x2 : Result2D -> Result2D
id1x2 input =
  case input of
    Qubit2D qs -> Qubit2D { up1 = qs.up1, down1 = qs.down1, up2 = qs.down2, down2 = qs.up2}
    Prob2D ps -> Prob2D { up_up = ps.up_down, up_down = ps.up_up , down_up = ps.down_down, down_down = ps.down_up} 
    _ -> input

x1id2 : Result2D -> Result2D
x1id2 input = swap2D (id1x2 (swap2D input))

{- straightforward stuff, just todo-}
id1y2 : Result2D -> Result2D
id1y2 = Debug.todo "todo"
y1id2 : Result2D -> Result2D
y1id2 input = swap2D (id1y2 (swap2D input))

id1z2 : Result2D -> Result2D
id1z2 = Debug.todo "todo"
z1id2 : Result2D -> Result2D
z1id2 input = swap2D (id1z2 (swap2D input))

id1h2 : Result2D -> Result2D
id1h2 = Debug.todo "todo"
h1id2 : Result2D -> Result2D
h1id2 input = swap2D (id1h2 (swap2D input))

x1x2 : Result2D -> Result2D
x1x2  = Debug.todo "todo"

x1y2 : Result2D -> Result2D
x1y2 = Debug.todo "todo"
y1x2 : Result2D -> Result2D
y1x2 input = swap2D (x1y2 (swap2D input))

x1z2 : Result2D -> Result2D
x1z2 = Debug.todo "todo"
z1x2 : Result2D -> Result2D
z1x2 input = swap2D (x1z2 (swap2D input))

x1h2 : Result2D -> Result2D
x1h2 = Debug.todo "todo"
h1x2 : Result2D -> Result2D
h1x2 input = swap2D (x1h2 (swap2D input))

y1y2 : Result2D -> Result2D
y1y2 = Debug.todo "todo"

y1z2 : Result2D -> Result2D
y1z2 = Debug.todo "todo"
z1y2 : Result2D -> Result2D
z1y2 input = swap2D (y1z2 (swap2D input))

y1h2 : Result2D -> Result2D
y1h2 = Debug.todo "todo"
h1y2 : Result2D -> Result2D
h1y2 input = swap2D (y1h2 (swap2D input))

y1c2 : Result2D -> Result2D
y1c2 = Debug.todo "todo"
c1y2 : Result2D -> Result2D
c1y2 input = swap2D (y1c2 (swap2D input))

z1z2 : Result2D -> Result2D
z1z2 = Debug.todo "todo"

z1h2 : Result2D -> Result2D
z1h2 = Debug.todo "todo"
h1z2 : Result2D -> Result2D
h1z2 input = swap2D (z1h2 (swap2D input))

z1c2 : Result2D -> Result2D
z1c2 = Debug.todo "todo"
c1z2 : Result2D -> Result2D
c1z2 input = swap2D (z1c2 (swap2D input))

h1h2 : Result2D -> Result2D
h1h2 = Debug.todo "todo"

h1c2 : Result2D -> Result2D
h1c2 = Debug.todo "todo"
c1h2 : Result2D -> Result2D
c1h2 input = swap2D (h1c2 (swap2D input))

c1x2 : Result2D -> Result2D
c1x2 input =
  case input of
    Qubit2D qs -> Prob2D { up_up = qs.up1 * qs.up2, up_down = qs.up1 * qs.down2, down_up = qs.down1 * qs.down2, down_down = qs.down1 * qs.up2}
    Prob2D ps -> Prob2D { up_up = ps.up_up, up_down = ps.up_down, down_up = ps.down_down, down_down = ps.down_up}
    _ -> input

x1c2 : Result2D -> Result2D
x1c2 input = swap2D (c1x2 (swap2D input))

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

funcToGate2D : (String, String) -> (Result2D -> Result2D)
funcToGate2D (x,y) =
  case (x,y) of
    ("x","x")       -> x1x2
    ("x","y")       -> x1y2
    ("x","z")       -> x1z2
    ("x","h")       -> x1h2
    ("x","c")       -> x1c2
    ("x","null")    -> x1id2
    ("y","x")       -> y1x2
    ("y","y")       -> y1y2
    ("y","z")       -> y1z2
    ("y","h")       -> y1h2
    ("y","c")       -> y1c2
    ("y","null")    -> y1id2
    ("z","x")       -> z1x2
    ("z","y")       -> z1y2
    ("z","z")       -> z1z2
    ("z","h")       -> z1h2
    ("z","c")       -> z1c2
    ("z","null")    -> z1id2
    ("h","x")       -> h1x2
    ("h","y")       -> h1y2
    ("h","z")       -> h1z2
    ("h","h")       -> h1h2
    ("h","c")       -> h1c2
    ("h","null")    -> h1id2
    ("c","x")       -> c1x2
    ("c","y")       -> c1y2
    ("c","z")       -> c1z2
    ("c","h")       -> c1h2
    ("null","x")    -> id1x2
    ("null","y")    -> id1y2
    ("null","z")    -> id1z2
    ("null","h")    -> id1h2
    ("null","null") -> id1id2
    _               -> Debug.todo "Invalid gate input in 2D"

funcsToGates2D : Array (String, String) -> List (Result2D -> Result2D)
funcsToGates2D xs = 
  toList(map funcToGate2D xs)

apply2Qhelper : Result2D -> List (Result2D -> Result2D) -> Result2D
apply2Qhelper init funcs =
  case funcs of
    [] -> init
    func::rest -> apply2Qhelper (func init) rest

apply2Q : Result2D -> Array (String, String)  -> Result2D
apply2Q init funcs = apply2Qhelper init (funcsToGates2D funcs)


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
