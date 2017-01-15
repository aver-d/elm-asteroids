module Util ( allPairs
            , bigrams
            , countBy
            , frequencies
            , groupBy
            , roundTo
            , uniqBy ) where
import Dict
import Set


allPairs seq1 seq2 =
  concatMap (\x -> map ((,) x) seq2) seq1


bigrams : [a] -> [(a, a)]
bigrams seq = zip seq <| tail seq


countBy : (a -> comparable) -> [a] -> Dict.Dict comparable number
countBy f seq =
  foldl (\x dict ->
    let v = f x
        n = Dict.getOrElse 0 v dict
    in Dict.insert v (n+1) dict)
    Dict.empty seq


frequencies : [comparable] -> Dict.Dict comparable number
frequencies = countBy identity


roundTo places n =
  (n * 10.0^places |> round |> toFloat) / 10.0^places


uniqBy f seq =
  case seq of
    [] -> []
    x::[] -> [x]
    _  -> let aux set seq' =
              case seq' of
                [] -> []
                x::xs ->
                  let val = f x
                  in if | Set.member val set -> aux set xs
                        | otherwise          -> x :: aux (Set.insert val set) xs

          in aux Set.empty seq
