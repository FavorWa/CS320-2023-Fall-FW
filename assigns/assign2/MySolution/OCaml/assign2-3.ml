#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit
  
type
('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work ->
    let _ = foldleft xs 0 (fun i x -> work i x; i) in ()
;;