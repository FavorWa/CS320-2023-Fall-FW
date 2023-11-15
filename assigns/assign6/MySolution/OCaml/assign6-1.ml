#use "./../../../../classlib/OCaml/MyOCaml.ml";;


  type sexpr =
    | SInt of int        (* 1, 2, 3, 4 ...  *)
    | SAdd of sexpr list (* (add e1 e2 ...) *)
    | SMul of sexpr list (* (mul e1 e2 ...) *)


  let rec 
  sexpr_to_string (c: sexpr) : string =
    let rec 
    concat__space acc list =
      match list with
        | [] -> ac
        | [x] -> ac ^ sexpr_to_string x
        | h :: t -> concat__space (ac ^ sexpr_to_string h ^ " ")t
    in
    match w with
      | SInt d -> string_of_int d
      | SMul hs -> "(mul " ^ concat__space "" hs ^ ")"
      | SAdd hs -> "(add " ^ concat__space "" hs ^ ")"
      

  
    
  let rec parse_sexpr () : sexpr parser =
    parse_Sint () <|> parse_Add () <|> parse_Mul ()
  
  and parse_Sint () : sexpr parser =
    let* n = natural in
    pure (SInt n) << whitespaces
  
  and parse_Add () : sexpr parser =
    let* _ = keyword "(add" in
    let* es = many1' parse_sexpr in
    let* _ = keyword ")" in
    pure (SAdd es)
  
  and parse_Mul () : sexpr parser =
    let* _ = keyword "(mul" in
    let* es = many1' parse_sexpr in
    let* _ = keyword ")" in
    pure (SMul es)
  
  let sexpr_parse (s : string) : sexpr option =
    match string_parse (parse_sexpr ()) s with
    | Some (e, []) -> Some e
    | _ -> None
  
  