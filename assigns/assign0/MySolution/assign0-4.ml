let string_init = String.init;;
let string_length = String.length;;
let string_get(cs, i0) = String.get cs i0 ;;
let ord = Char.code;;

#use "./../assign0.ml";;


let isNegative (s: string) : bool = 
  if string_get(s, 0) = '-' then true
  else false

let rec str2int (cs: string) : int = 
  let len = string_length cs in
  if len = 0 then 0 else
    let pChar = string_get (cs, (len - 1)) in 
    let ind = if isNegative cs then 1 else 0 in
    let append = 
      string_init (len - 1) (fun i -> string_get (cs, ind + i))
    in 
    let res = str2int append in
    if isNegative cs then -((10 * res) + ord pChar - 48) else (10 * res) + ord pChar - 48
;;