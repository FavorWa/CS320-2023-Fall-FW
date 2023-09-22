#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_132(cs: string): bool = 
  let rec smaller(str: string)(i: int)(bigger: int): bool = 
    if string_length str = (i+1) then true
    else if ord(string_get_at str(i+1)) < largest then false
    else
    smaller str (i+1) (ord(string_get_at str (i+1)))
    in

  let rec next(i:int) (len: int): bool = 
    if (i+2) >= len then true
    else
      if ord(string_get_at cs i ) >= ord(string_get_at cs (i+1)) then
        next (i+1) len
      else
        smaller cs (i + 1) (ord(string_get_at cs (i+1))) in
  next 0 (string_length cs)

      ;;