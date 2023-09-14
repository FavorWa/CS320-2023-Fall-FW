#use "./../assign0.ml";;

(* ****** ****** *)

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)

let rec get (str: string)(ind:int) char = 
  let ch = str.[ind] in ch

  let rec stringrev(cs: string): string = 
  let len = string_length cs in
  if len = 0 then "" else
    let lastChar = len - 1 in 
    let rev = string_init len (fun i -> get cs (lastChar-1)) in rev
  ;;
