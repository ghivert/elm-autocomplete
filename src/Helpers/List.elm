module Helpers.List exposing (containsAll, merge, toIndexedList)

import Array

containsAll : List a -> List a -> Bool
containsAll l1 l2 =
  case l2 of
    hd :: tl ->
      if List.member hd l1 then
        containsAll l1 tl
      else
        False
    [] ->
      True

merge : List a -> List a -> List a
merge = mergeHelp << List.reverse

mergeHelp : List a -> List a -> List a
mergeHelp l1 l2 =
  case l1 of
    hd :: tl ->
      if List.member hd l2 then
        mergeHelp tl l2
      else
        mergeHelp tl (hd :: l2)
    [] ->
      l2

toIndexedList : List a -> List ( Int, a )
toIndexedList = Array.toIndexedList << Array.fromList
