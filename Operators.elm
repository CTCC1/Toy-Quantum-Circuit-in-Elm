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
      result = [  [a11*b11, a11*b12, a12*b11, a12*b12]
                , [a11*b21, a11*b22, a12*b21, a12*b22]
                , [a21*b11, a21*b12, a22*b11, a22*b12]
                , [a21*b21, a21*b22, a22*b21, a22*b22]]
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

notc : M.Matrix Float
notc =
  case M.fromLists [[1,0,0,0],[0,0,0,1],[0,0,1,0],[0,1,0,0]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

cz : M.Matrix Float
cz =
  case M.fromLists [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,-1]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

zc : M.Matrix Float
zc = cz {-cz and zc both only phase 11-}

ch : M.Matrix Float
ch =
  case M.fromLists [[1,0,0,0],[0,1,0,0],[0,0,1 / sqrt(2),1 / sqrt(2)],[0,0,1 / sqrt(2),-1 / sqrt(2)]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

hc : M.Matrix Float
hc = 
  case M.fromLists [[1,0,0,0],[0,1 / sqrt(2),0,1 / sqrt(2)],[0,0,1,0],[0,1 / sqrt(2),0,-1 / sqrt(2)]] of
    Just xn -> xn
    _       -> Debug.todo "impossible"

-- 2 Qubit gate is 4*4 matrix, i.e. Tensor product of 2 2*2 matrices(representing the 1Q gate) --
---standard bipartite spin-1/2 basis: {00,01,10,11}---

strToGate2Q : (String, String) -> M.Matrix Float
strToGate2Q (str1, str2) =
  case (str1, str2) of
    ("c","c")    -> Debug.todo "invalid gate, two control in 2Q"
    ("c","null") -> Debug.todo "invalid gate, empty control in 2Q"
    ("null","c") -> Debug.todo "invalid gate, empty control in 2Q"
    ("c","x")    -> cnot
    ("c","z")    -> cz
    ("c","h")    -> ch
    ("x","c")    -> notc
    ("z","c")    -> zc
    ("h","c")    -> hc
    _            -> tensorProduct22 (strToGate1Q str1) (strToGate1Q str2) 

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

validP : Float -> Bool
validP x = (x >= 0) && (x <= 1)

prep2Q : (Float, Float) -> M.Matrix Float
prep2Q (a1,a2) =
  if (validP a1) && (validP a2) then
    let
      b1 = sqrt(1 - a1 * a1)
      b2 = sqrt(1 - a2 * a2)
    in
      case M.fromLists [[a1*a2], [a1*b2], [a2*b1], [b1*b2]] of
        Just v -> v
        _      -> Debug.todo "impossible"
  else
    Debug.todo "invalid coefficient"

apply2Q : (Float, Float) -> Array (String, String)  -> M.Matrix Float
apply2Q init funcs = apply2Qhelper (prep2Q init) (strsToGates2Q funcs)

measure2Q : M.Matrix Float -> List Float
measure2Q r =
  case M.size r of
    (4, 1) -> 
      let
        rarr = fromList <| M.toList <| r
      in
        [unsafeGet 0 rarr, unsafeGet 1 rarr, unsafeGet 2 rarr, unsafeGet 3 rarr]
    _      -> Debug.todo "impossible"

------ Tensor Product between a 4*4 matrices and a 2*2 matrix (forming 3 Qubit Gates)------

tensorProduct42 : M.Matrix Float -> M.Matrix Float -> M.Matrix Float
tensorProduct42 x y = 
  if M.size x == (4,4) && M.size y == (2,2) then
    let
      xarr = fromList <| M.toList <| x
      yarr = fromList <| M.toList <| y
      a11 = unsafeGet 0 xarr
      a12 = unsafeGet 1 xarr
      a13 = unsafeGet 2 xarr
      a14 = unsafeGet 3 xarr
      a21 = unsafeGet 4 xarr
      a22 = unsafeGet 5 xarr
      a23 = unsafeGet 6 xarr
      a24 = unsafeGet 7 xarr
      a31 = unsafeGet 8 xarr
      a32 = unsafeGet 9 xarr
      a33 = unsafeGet 10 xarr
      a34 = unsafeGet 11 xarr
      a41 = unsafeGet 12 xarr
      a42 = unsafeGet 13 xarr
      a43 = unsafeGet 14 xarr
      a44 = unsafeGet 15 xarr
      b11 = unsafeGet 0 yarr
      b12 = unsafeGet 1 yarr
      b21 = unsafeGet 2 yarr
      b22 = unsafeGet 3 yarr
      result = [  [a11*b11, a11*b12, a12*b11, a12*b12, a13*b11, a13*b12, a14*b11, a14*b12]
                , [a11*b21, a11*b22, a12*b21, a12*b22, a13*b21, a13*b22, a14*b21, a14*b22]
                , [a21*b11, a21*b12, a22*b11, a22*b12, a23*b11, a23*b12, a24*b11, a24*b12]
                , [a21*b21, a21*b22, a22*b21, a22*b22, a23*b21, a23*b22, a24*b21, a24*b22]
                , [a31*b11, a31*b12, a32*b11, a32*b12, a33*b11, a33*b12, a34*b11, a34*b12]
                , [a31*b21, a31*b22, a32*b21, a32*b22, a33*b21, a33*b22, a34*b21, a34*b22]
                , [a41*b11, a41*b12, a42*b11, a42*b12, a43*b11, a43*b12, a44*b11, a44*b12]
                , [a41*b21, a41*b22, a42*b21, a42*b22, a43*b21, a43*b22, a44*b21, a44*b22]]
    in
      case M.fromLists result of
        Just r  -> r
        Nothing -> Debug.todo "error in tensorProduct42"
  else
    Debug.todo "error in tensorProduct42, wrong matrix size"

------ Tensor Product between a 2*2 matrices and a 4*4 matrix (forming 3 Qubit Gates)------

tensorProduct24 : M.Matrix Float -> M.Matrix Float -> M.Matrix Float
tensorProduct24 x y = 
  if M.size x == (2,2) && M.size y == (4,4) then
    let
      xarr = fromList <| M.toList <| x
      yarr = fromList <| M.toList <| y
      a11 = unsafeGet 0 xarr
      a12 = unsafeGet 1 xarr
      a21 = unsafeGet 2 xarr
      a22 = unsafeGet 3 xarr
      b11 = unsafeGet 0 yarr
      b12 = unsafeGet 1 yarr
      b13 = unsafeGet 2 yarr
      b14 = unsafeGet 3 yarr
      b21 = unsafeGet 4 yarr
      b22 = unsafeGet 5 yarr
      b23 = unsafeGet 6 yarr
      b24 = unsafeGet 7 yarr
      b31 = unsafeGet 8 yarr
      b32 = unsafeGet 9 yarr
      b33 = unsafeGet 10 yarr
      b34 = unsafeGet 11 yarr
      b41 = unsafeGet 12 yarr
      b42 = unsafeGet 13 yarr
      b43 = unsafeGet 14 yarr
      b44 = unsafeGet 15 yarr
      result = [  [a11*b11, a11*b12, a11*b13, a11*b14, a12*b11, a12*b12, a12*b13, a12*b14]
                , [a11*b21, a11*b22, a11*b23, a11*b24, a12*b21, a12*b22, a12*b23, a12*b24]
                , [a11*b31, a11*b32, a11*b33, a11*b34, a12*b31, a12*b32, a12*b33, a12*b34]
                , [a11*b41, a11*b42, a11*b43, a11*b44, a12*b41, a12*b42, a12*b43, a12*b44]
                , [a21*b11, a21*b12, a21*b13, a21*b14, a22*b11, a22*b12, a22*b13, a22*b14]
                , [a21*b21, a21*b22, a21*b23, a21*b24, a22*b21, a22*b22, a22*b23, a22*b24]
                , [a21*b31, a21*b32, a21*b33, a21*b34, a22*b31, a22*b32, a22*b33, a22*b34]
                , [a21*b41, a21*b42, a21*b43, a21*b44, a22*b41, a22*b42, a22*b43, a22*b44]]
    in
      case M.fromLists result of
        Just r  -> r
        Nothing -> Debug.todo "error in tensorProduct24"
  else
    Debug.todo "error in tensorProduct24, wrong matrix size"

{-
   3 Qubit gate is 8*8 matrix, i.e. Tensor product of three 2*2 matrices
   We do not support two control gates like Tofolli at the moment --
   We use cu(up) and cd (down) to indicate which adjacent control gate is connected to;
   If we want to do control gate between 1st and 3rd, we demand the 2nd to be "bridge"!
-}

---3 qubit state basis: {000,001,010,011,100,101,110,111}---

c1id2x3 : M.Matrix Float
c1id2x3 = 
  let
    m88 = [ [1,0,0,0,0,0,0,0]
          , [0,1,0,0,0,0,0,0]
          , [0,0,1,0,0,0,0,0]
          , [0,0,0,1,0,0,0,0]
          , [0,0,0,0,0,1,0,0]
          , [0,0,0,0,1,0,0,0]
          , [0,0,0,0,0,0,0,1]
          , [0,0,0,0,0,0,1,0]]
  in
    case M.fromLists m88 of
      Just xn -> xn
      _       -> Debug.todo "impossible"

c1id2z3 : M.Matrix Float
c1id2z3 = 
  let
    m88 = [ [1,0,0,0,0,0, 0,0 ]
          , [0,1,0,0,0,0, 0,0 ]
          , [0,0,1,0,0,0, 0,0 ]
          , [0,0,0,1,0,0, 0,0 ]
          , [0,0,0,0,1,0, 0,0 ]
          , [0,0,0,0,0,-1,0,0 ]
          , [0,0,0,0,0,0, 1,0 ]
          , [0,0,0,0,0,0, 0,-1]]
  in
    case M.fromLists m88 of
      Just xn -> xn
      _       -> Debug.todo "impossible"

c1id2h3 : M.Matrix Float
c1id2h3 = 
  let
    m88 = [ [1,0,0,0,        0,         0,        0,        0 ]
          , [0,1,0,0,        0,         0,        0,        0 ]
          , [0,0,1,0,        0,         0,        0,        0 ]
          , [0,0,0,1,        0,         0,        0,        0 ]
          , [0,0,0,0,1/sqrt(2), 1/sqrt(2),        0,        0 ]
          , [0,0,0,0,1/sqrt(2),-1/sqrt(2),        0,        0 ]
          , [0,0,0,0,        0,         0,1/sqrt(2), 1/sqrt(2)]
          , [0,0,0,0,        0,         0,1/sqrt(2),-1/sqrt(2)]]
  in
    case M.fromLists m88 of
      Just xn -> xn
      _       -> Debug.todo "impossible"

x1id2c3 : M.Matrix Float
x1id2c3 = 
  let
    m88 = [ [1,0,0,0,0,0,0,0]
          , [0,0,0,0,0,1,0,0]
          , [0,0,1,0,0,0,0,0]
          , [0,0,0,0,0,0,0,1]
          , [0,0,0,0,1,0,0,0]
          , [0,1,0,0,0,0,0,0]
          , [0,0,0,0,0,0,1,0]
          , [0,0,0,1,0,0,0,0]]
  in
    case M.fromLists m88 of
      Just xn -> xn
      _       -> Debug.todo "impossible"

z1id2c3 : M.Matrix Float
z1id2c3 = c1id2z3

h1id2c3 : M.Matrix Float
h1id2c3 = 
  let
    m88 = [ [1,        0,0,        0,0,         0,0,0         ]
          , [0,1/sqrt(2),0,        0,0, 1/sqrt(2),0,0         ]
          , [0,        0,1,        0,0,         0,0,0         ]
          , [0,        0,0,1/sqrt(2),0,         0,0,1/sqrt(2) ]
          , [0,        0,0,        0,1,         0,0,0         ]
          , [0,1/sqrt(2),0,        0,0,-1/sqrt(2),0,0         ]
          , [0,        0,0,        0,0,         0,1,0         ]
          , [0,        0,0,1/sqrt(2),0,         0,0,-1/sqrt(2)]]
  in
    case M.fromLists m88 of
      Just xn -> xn
      _       -> Debug.todo "impossible"


strToGate3Q : (String, String, String) -> M.Matrix Float
strToGate3Q (str1, str2, str3) =
  case (str1, str2, str3) of
    ("cd","bridge",_)  ->
      case str3 of
        "x" -> c1id2x3
        "z" -> c1id2z3
        "h" -> c1id2h3
        _   -> Debug.todo "impossible"
    (_, "bridge","cu") ->
      case str1 of
        "x" -> x1id2c3
        "z" -> z1id2c3
        "h" -> h1id2c3
        _   -> Debug.todo "impossible"
    ("cd",_,_)         ->
      case str2 of
        "x" -> tensorProduct42 cnot (strToGate1Q str3)
        "z" -> tensorProduct42 cz   (strToGate1Q str3)
        "h" -> tensorProduct42 ch   (strToGate1Q str3)
        _   -> Debug.todo "impossible"
    (_,_,"cu")         ->
      case str2 of
        "x" -> tensorProduct24 (strToGate1Q str1) notc
        "z" -> tensorProduct24 (strToGate1Q str1) zc
        "h" -> tensorProduct24 (strToGate1Q str1) hc
        _   -> Debug.todo "impossible"
    (_,"cu",_)         ->
      case str1 of
        "x" -> tensorProduct42 notc (strToGate1Q str3)
        "z" -> tensorProduct42 zc   (strToGate1Q str3)
        "h" -> tensorProduct42 hc   (strToGate1Q str3)
        _   -> Debug.todo "impossible"
    (_,"cd",_)         ->
      case str3 of
        "x" -> tensorProduct24 (strToGate1Q str1) cnot
        "z" -> tensorProduct24 (strToGate1Q str1) cz
        "h" -> tensorProduct24 (strToGate1Q str1) ch
        _   -> Debug.todo "impossible"
    _            -> tensorProduct42 (tensorProduct22 (strToGate1Q str1) (strToGate1Q str2)) (strToGate1Q str3)

strsToGates3Q : Array (String, String, String) -> List (M.Matrix Float)
strsToGates3Q xs = 
  toList(map strToGate3Q xs)

apply3Qhelper : M.Matrix Float -> List (M.Matrix Float) -> M.Matrix Float
apply3Qhelper init funcs =
  case funcs of
    []      -> init
    m::rest -> 
      case (M.dot m init) of
        Just result -> apply3Qhelper result rest
        Nothing     -> Debug.todo "invalid operations in apply3Q"

prep3Q : (Float, Float, Float) -> M.Matrix Float
prep3Q (a1,a2,a3) =
  if (validP a1) && (validP a2) && (validP a3) then
    let
      b1 = sqrt(1 - a1 * a1)
      b2 = sqrt(1 - a2 * a2)
      b3 = sqrt(1 - a3 * a3)
      vector = [ [a1*a2*a3], [a1*a2*b3], [a1*b2*a3], [a1*b2*b3]
              , [b1*a2*a3], [b1*a2*b3], [b1*b2*a3], [b1*b2*b3]]
    in
      case M.fromLists vector of
        Just v -> v
        _      -> Debug.todo "prep3Q impossible"
  else
    Debug.todo "invalid coefficient"

apply3Q : (Float, Float, Float) -> Array (String, String, String)  -> M.Matrix Float
apply3Q init funcs = apply3Qhelper (prep3Q init) (strsToGates3Q funcs)

measure3Q : M.Matrix Float -> List Float
measure3Q r =
  case M.size r of
    (8, 1) -> 
      let
        rarr = fromList <| M.toList <| r
      in
        [ unsafeGet 0 rarr, unsafeGet 1 rarr, unsafeGet 2 rarr, unsafeGet 3 rarr
        , unsafeGet 4 rarr, unsafeGet 5 rarr, unsafeGet 6 rarr, unsafeGet 7 rarr]
    _      -> Debug.todo "measure3Q impossible"
