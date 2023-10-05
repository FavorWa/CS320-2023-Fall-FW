#use " ./../../assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_subsets(xs: 'a list): 'a list list = 
  let set = [[]] in 
  let subset = 
    List.fold_left
    (fun acc x ->
      let newsub = List.map (fun subset  -> x :: subset) acc in 
      acc @ newsub)
    set
    xs
  in
  subset

;;