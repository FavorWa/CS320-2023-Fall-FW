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

let neg(str: string): bool = 
if str.[0] = '-' then true else false


let rec str2int(cs: string): int = 
let lenOfStr = string_length cs in 
if lenOfStr = 0 then 0 else
  let lastChar = string_get(cs, lenOfStr - 1) in
  let ordValue = ord lastChar - 48 in
  let ind = if neg(cs) = true then 1 else 0 in
  let add = string_init (lenOfStr-1)(fun i -> string_get(cs,ind + i )) in 
  let newS = str2int(add) in 
  if neg(cs) then -(((10 * newS) + ordValue)) else (10*newS) + ordValue
;;
