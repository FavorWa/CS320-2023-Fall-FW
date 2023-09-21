let string_init = String.init;;
let string_length = String.length;;
let string_get(cs, i0) = String.get cs i0 ;;
let chr = Char.chr;;
let ord = Char.code;;



#use "./../assign0.ml";;

let append (ch: char) (cs: string): string = 
  string_init (string_length cs + 1) (fun i ->
    if i = 0 then ch else string_get(cs, i-1)
  )

let rec num2str2 (i:int) (s: string): string = 
  if i < 10 then 
    append (chr (ord '0' + i)) s
  else
    let n0 = i mod 10 in
    let rest_int = i / 10 in
    num2str2 rest_int (append (chr (ord '0' + n0)) s)

let num2str(i:int): string = 
  if i = 0 then "0" 
  else
    num2str2 i ""

let int2str (i0: int): string =
  if i0 < 0 then 
      append '-' (num2str (-i0))
  else
    num2str i0

  ;;
