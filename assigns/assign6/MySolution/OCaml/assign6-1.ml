(**
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
sexpr_parse "(add 1 2 3)" = Some (SAdd [SInt 1; SInt 2; Int 3])
sexpr_parse "(mul (add 1 2) 3 (mul 1))" = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
//
Example (Rejected Strings):
sexpr_parse "()" = None
sexpr_parse "(add)" = None
sexpr_parse "(add 1 2))" = None
sexpr_parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )

*)


type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *) 


  let rec
  list_sexpr (s: sexpr list) : string =
  match s with 
  | (SInt h) :: [] -> (string_of_int h) ^ ")"
  | (SInt h) :: t -> (string_of_int h) ^ " " ^ (list_sexpr t)
  | (SAdd a) :: [] -> (sexpr_to_string (SAdd a)) ^ (list_sexpr [])
  | (SAdd a) :: t -> (sexpr_to_string (SAdd a)) ^ " " ^ (list_sexpr t)
  | (SMul a) :: [] -> (sexpr_to_string (SMul a)) ^ (list_sexpr [])
  | (SMul a) :: t -> (sexpr_to_string (SMul a)) ^ " " ^ (list_sexpr t)
  | [] -> ")"


and sexpr_to_string (expr: sexpr) : string =
  match expr with
  | SInt a -> string_of_int a
  | SAdd [] | SMul [] -> ""
  | SAdd (h :: t) -> (match h , t with 
    | (SInt a, t) -> "(add " ^ (string_of_int a) ^ " " ^ (list_sexpr t)
    | ((SAdd a | SMul a), t) -> "(add " ^ (list_sexpr a) ^ " " ^ (list_sexpr a))
  | SMul (h :: t) -> (match h, t with 
    | (SInt a, t) -> "(mul " ^ (string_of_int a) ^ " " ^ (list_sexpr t)
    | ((SAdd a | SMul a), t) -> "(add " ^ (list_sexpr a) ^ " " ^ (list_sexpr a))
  
let rec parse_sexpr () : sexpr parser =
  parse_Sint () <|> parse_Add () <|> parse_Mul ()

and parse_Sint () : sexpr parser = 
  let* n = natural in
  pure (SInt n) << whitespaces

and parse_Add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SAdd es)

and parse_Mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SMul es)

let sexpr_parse (s: string) : sexpr option = 
  match string_parse (parse_sexpr ()) s with
  | Some (e, []) -> Some e
  | _ -> None
