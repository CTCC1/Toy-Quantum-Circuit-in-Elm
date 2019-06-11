module Kronecker exposing (..)

import List exposing (map, map2, foldl)

oneline : List number -> List (List number) -> List (List number)
oneline ai b = 
  let 
    oneblock aii y =
      case y of
        [] -> []
        yj::rest -> (map (\x -> x * aii) yj) :: (oneblock aii rest)
    recur x y =
      case x of
        [] -> []
        aii::rest -> (oneblock aii y)::(recur rest y)
    mergeblocks x y = map2 (++) y x
  in
    case (recur ai b) of
      [] -> []
      a1::rest -> foldl mergeblocks a1 rest

kroneckerProduct : List (List number) -> List (List number) -> List (List number)
kroneckerProduct a b =
  case (a, b) of
    ([], _) -> []
    (ai::rest, _) -> (oneline ai b) ++ (kroneckerProduct rest b)
