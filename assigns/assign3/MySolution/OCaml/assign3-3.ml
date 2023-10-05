#use " ./../../assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;


let list_nchoose (xs: 'a list) (n0:int) : 'a list list = 
  let beg = [[]] in
  let append acc x =
    List.concat_map  (fun sub ->
      if List.length sub = n0 then [sub] else [x :: sub; sub]
      ) acc
    in
    let res = List.fold_left append beg xs in
    List.filter (fun sub -> List.length sub = n0) res
    ;;