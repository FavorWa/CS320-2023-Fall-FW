#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

  let mylist_subscript_exn () : 'a = raise MySubscript;;

let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons (_, tail) ->  mylist_length tail + 1 
  | MySnoc (mystr, _ ) -> (mylist_length mystr) + 1
  | MyAppend2 (l1, l2) -> (mylist_length l1) + (mylist_length l2)
  | MyReverse list' -> mylist_length list'

let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
    match xs with 
    | MyNil -> mylist_subscript_exn()
    | MyCons (head, str) ->
      if i0 < 0 then mylist_subscript_exn()
      else if i0 = 0 then head
      else mylist_get_at str (i0-1)

    | MySnoc (str, elem) ->
      if i0 = (mylist_length str) then elem 
      else mylist_get_at(str) (i0-1)

    | MyAppend2 (list1, list2) ->
      let lenList1 = mylist_length list1 in
      if i0 < 0 then mylist_subscript_exn()
      else if i0 < lenList1 then mylist_get_at list1 i0
      else mylist_get_at list2 (i0 - lenList1)

    | MyReverse list' -> 
      let lenList = mylist_length list' in 
      let reverseIndex = (lenList - i0 - 1) in
      if i0 < 0 || reverseIndex < 0 then mylist_subscript_exn()
      else mylist_get_at list' reverseIndex

    ;;