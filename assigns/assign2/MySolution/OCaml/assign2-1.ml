#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist


let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons (_, tail) ->  mylist_length tail + 1 
  | MySnoc (mystr, _ ) -> (mylist_length mystr) + 1
  | MyAppend2 (l1, l2) -> (mylist_length l1) + (mylist_length l2)
  | MyReverse list' -> mylist_length list'

;;