#use " ./../../assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec helper (matrix: 'a list list): 'a list list =
  match matrix with 
  | [] -> []
  | []::_ -> []
  | _ ->
    let first = List.map List.hd matrix in
    let next = helper (List.map List.tl matrix) in
    first :: next

let matrix_transpose (xss: 'a list list) : ' a list list = 
  helper xss 

;;