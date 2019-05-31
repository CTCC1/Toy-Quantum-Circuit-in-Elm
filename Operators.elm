module Operators exposing (..)

import Array exposing (Array(..), toList, fromList, map, get)
import Matrix as M
{- fromLists, dot, toList-}

---Single Qubit Gates---

id1Q : M.Matrix Float
id1Q = M.identity 2

xgate1Q : M.Matrix Float
xgate1Q = 
  case M.fromLists [[0,1],[1,0]] of
    Just x -> x
    _      -> Debug.todo "impossible"

zgate1Q : M.Matrix Float
zgate1Q = 
  case M.fromLists [[1,0],[0,-1]] of
    Just z -> z
    _      -> Debug.todo "impossible"

hadamard1Q : M.Matrix Float
hadamard1Q =
  case M.fromLists [[1 / sqrt(2),1 / sqrt(2)],[1 / sqrt(2),-1 / sqrt(2)]] of
    Just z -> z
    _      -> Debug.todo "impossible"

--- Tensor Product between 2 2*2 Matrices, result in 4*4 Matrices ---
unsafeGet :  Int -> Array Float -> Float
unsafeGet i arr =
  case get i arr of
    Just x -> x
    Nothing -> Debug.todo "error in unsafeGet"

tensorProduct22 : M.Matrix Float -> M.Matrix Float -> M.Matrix Float
tensorProduct22 x y = 
  if M.size x == (2,2) && M.size y == (2,2) then
    let
      xarr = fromList <| M.toList <| x
      yarr = fromList <| M.toList <| y
      a11 = unsafeGet 0 xarr
      a12 = unsafeGet 1 xarr
      a21 = unsafeGet 2 xarr
      a22 = unsafeGet 3 xarr
      b11 = unsafeGet 0 yarr
      b12 = unsafeGet 1 yarr
      b21 = unsafeGet 2 yarr
      b22 = unsafeGet 3 yarr
      result = [[a11*b11, a11*b12, a12*b11, a12*b12], [a11*b21, a11*b22, a12*b21, a12*b22], [a21*b11, a21*b12, a22*b11, a22*b12], [a21*b21, a21*b22, a22*b21, a22*b22]]
    in
      case M.fromLists result of
        Just r  -> r
        Nothing -> Debug.todo "error in tensorProduct22"
  else
    Debug.todo "error in tensorProduct22, wrong matrix size"

---2-Qubit Gates---

id1id2 : M.Matrix Float
id1id2 = tensorProduct22 id1Q id1Q

strToGate1Q : String -> M.Matrix Float
strToGate1Q str =
  case str of
    "null" -> id1Q
    "x"    -> xgate1Q
    "z"    -> zgate1Q
    "h"    -> hadamard1Q
    "c"    -> Debug.todo "shouldn't reach there strToGate1Q"
    _      -> Debug.todo "invalid gate in strToGate1Q"

cnot : M.Matrix Float
cnot =
  case M.fromLists [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

cz : M.Matrix Float
cz =
  case M.fromLists [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,-1]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

ch : M.Matrix Float
ch =
  case M.fromLists [[1,0,0,0],[0,1,0,0],[0,0,1 / sqrt(2),1 / sqrt(2)],[0,0,1 / sqrt(2),-1 / sqrt(2)]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

-- 2 Qubit gate is 4*4 matrix, i.e. Tensor product of 2 2*2 matrices(representing the 1Q gate) --
strToGate2Q : (String, String) -> M.Matrix Float
strToGate2Q (str1, str2) =
  case str1 of
    "c" ->
      case str2 of
        "c" -> Debug.todo "invalid gate, two control in 2Q"
        "x" -> cnot
        "z" -> cz
        "h" -> ch
        _   -> Debug.todo "invalid gate in strsToGate2Q"
    _   -> tensorProduct22 (strToGate1Q str1) (strToGate1Q str2) 

strsToGates2Q : Array (String, String) -> List (M.Matrix Float)
strsToGates2Q xs = 
  toList(map strToGate2Q xs)

apply2Qhelper : M.Matrix Float -> List (M.Matrix Float) -> M.Matrix Float
apply2Qhelper init funcs =
  case funcs of
    []      -> init
    m::rest -> 
      case (M.dot m init) of
        Just result -> apply2Qhelper result rest
        Nothing     -> Debug.todo "invalid operations in apply2Q"

prep2Q : (Float, Float) -> M.Matrix Float
prep2Q (a1,a2) =
  let
    b1 = sqrt(1 - a1 * a1)
    b2 = sqrt(1 - a2 * a2)
  in
    case M.fromLists [[a1*a2], [a1*b2], [a2*b1], [b1*b2]] of
    Just v -> v
    _      -> Debug.todo "impossible"

apply2Q : (Float, Float) -> Array (String, String)  -> M.Matrix Float
apply2Q init funcs = apply2Qhelper (prep2Q init) (strsToGates2Q funcs)

type alias Result2Q =
  { downdown: Float
  , downup: Float
  , updown: Float
  , upup: Float
  }

measure2Q : M.Matrix Float -> Result2Q
measure2Q r =
  case M.size r of
    (4, 1) -> 
      let
        rarr = fromList <| M.toList <| r
      in
      { downdown = unsafeGet 0 rarr, downup = unsafeGet 1 rarr, updown = unsafeGet 2 rarr, upup = unsafeGet 3 rarr}
    _      -> Debug.todo "impossible"

---standard bipartite spin-1/2 basis: {00,01,10,11}---
{-

type Result1D = Qubit  { up :Float, down : Float} | Prob1D { up :Float, down : Float }

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

type Result2D = Qubit2D { up1 :Float, down1 : Float , up2 : Float, down2 : Float} | Prob2D { up_up :Float, up_down : Float , down_up : Float, down_down : Float} | Output2D { up1 :Float, down1 : Float , up2 : Float, down2 : Float}

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


------3 Qubits Gate---
--- Tensor Product between a 2*2 Matrix and a 4*4 Matrix, result in 8*8 matrix ---


--- Tensor Product between a 4*4 Matrix and a 2*2 Matrix, result in 8*8 matrix ---
-}