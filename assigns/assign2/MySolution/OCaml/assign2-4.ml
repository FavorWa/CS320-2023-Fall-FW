#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec string_sepjoin_list sep xs =
  match xs with
  | [] -> ""
  | [x] -> x
  | head::tail -> string_concat_list [head; sep; string_sepjoin_list sep tail]

;;