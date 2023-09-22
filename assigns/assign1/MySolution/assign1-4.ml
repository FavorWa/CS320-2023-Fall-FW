#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml"


let intrep_add ds1 ds2 =
  let rec add_with_carry carry idx1 idx2 result =
    if idx1 < 0 && idx2 < 0 then
      if carry = 1 then "1" ^ result else result
    else
      let digit1 = if idx1 >= 0 then digit_of_char ds1.[idx1] - digit_of_char '0' else 0 in
      let digit2 = if idx2 >= 0 then digit_of_char ds2.[idx2] - digit_of_char '0' else 0 in
      let sum = digit1 + digit2 + carry in
      let new_carry = sum / 10 in
      let new_digit = sum mod 10 in
      add_with_carry new_carry (idx1 - 1) (idx2 - 1) (string_of_int new_digit ^ result)
  in
  let len1 = String_length ds1 in
  let len2 = String_length ds2 in
  let result = add_with_carry 0 (len1 - 1) (len2 - 1) "" in
  if result = "" then "0" else result
;;



















(*let pad (str: string) (diff: int): string =
  let padding = String.make diff '0' in
  padding ^ str

let intrep_add(ds1: string)(ds2: string): string =
  let newds1, newds2 =
    if String.length ds1 < String.length ds2 then
      let padded_ds1 = pad ds1 (String.length ds2 - String.length ds1) in
      (padded_ds1, ds2)
    else
      let padded_ds2 = pad ds2 (String.length ds1 - String.length ds2) in
      (ds1, padded_ds2)
  in

  let rec add_with_carry (ds1: int) (ds2: int) (carry: int) (i: int )int = 
    let result = 
      if i < 0 then
        if carry = 0 then 
          char_of_digit (digit_of_char (String.get newds1 i) + digit_of_char (String.get newds2 i))
    else
      char_of_digit carry ^ char_of_digit (digit_of_char (String.get newds1 i) + digit_of_char (String.get newds2 i))
      else
        let sum = digit_of_char (String.get newds1 i) + digit_of_char (String.get newds2 i) + carry in
        let new_carry = sum / 10 in
        let digit_sum = sum mod 10 in

      

  string_make_fwork( fun work ->
    int1_rforeach (String.length newds2)(fun i ->
       work (char_of_digit (digit_of_char (String.get newds1 i) + digit_of_char (String.get newds2 i)));
    )
  )

  *)

      
      

    
