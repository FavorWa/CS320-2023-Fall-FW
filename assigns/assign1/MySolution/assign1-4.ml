#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml"

let pad (str: string) (diff: int): string =
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
  string_make_fwork( fun work ->
    int1_rforeach (String.length newds2)(fun i ->
       work (char_of_digit (digit_of_char (String.get newds1 i) + digit_of_char (String.get newds2 i)));
    )
  ) 
      
      

    
