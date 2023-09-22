#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml"


let rec sort_strings cs1 cs2 =
  match cs1, cs2 with 
  | "", "" -> ""
  | "", cs2' -> cs2'
  | cs1', "" -> cs1'
  | _ ->

    let ch1 = string_head cs1 in
    let ch2 = string_head cs2 in
    if ch1 <= ch2 then
      string_cons ch1 (sort_strings (string_tail cs1) cs2)
    else
      string_cons ch1 (sort_strings cs1 (string_tail cs2))
    

let string_merge cs1 cs2 = 
  let merge = sort_strings cs1  cs2 in 
  let filter = string_filter merge (fun c -> true) in
  string_concat_list[filter]
;;