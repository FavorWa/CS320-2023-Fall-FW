#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const =
  | Int of int
  | Bool of bool
  | Unit

type com =
  | Push of const
  | Pop
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt

type prog = com list

type trace = string list

type stack = const list

(*UTIL*)
let is_digit c =
  '0' <= c && c <= '9'
let explode s =
  List.of_seq (String.to_seq s)
let implode ls =
  String.of_seq (List.to_seq ls)
 
  let boolToString (b : bool) : string =
    if b then "True" else "False"
  
  let toString (c : const) : string =
    match c with
    | Int i -> string_of_int i
    | Bool b -> boolToString b
    | Unit -> "Unit"
(*UTIL*)

(* parser combinators *)
type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)
let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)
let fail : 'a parser = fun ls -> None
let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None
let (>>=) = bind
let (let*) = bind
let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None
let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None
let char (c : char) : char parser =
  satisfy (fun x -> x = c)
let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None
let (>>) = seq
let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None
let (<<) = seq'
let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls
let (<|>) = disj
let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None
let (>|=) = map
let (>|) = fun p c -> map p (fun _ -> c)
let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)
let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None
let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)
let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None
let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None
let ws : unit parser =
  (many whitespace) >| ()
let ws1 : unit parser =
  (many1 whitespace) >| ()
let digit : char parser =
  satisfy is_digit
let option (p : 'a parser) : 'a option parser =
  fun ls ->
    match p ls with
    | Some (x, ls') -> Some (Some x, ls')
    | None -> Some (None, ls)
(***End of parser combinators*)


let digit : int parser =
  let* d = satisfy is_digit in
  pure (int_of_char d - int_of_char '0')

let nat : int parser =
  let* ds = many1 digit in
  pure (List.fold_left (fun n d -> 10 * n + d) 0 ds)

let integer : int parser =
  let* sign = option (char '-') in
  let* n = nat in
  pure (match sign with Some _ -> -n | None -> n)

let bool : bool parser =
  (keyword "True" >| true) <|> (keyword "False" >| false)

  let const : const parser =
    (integer >|= (fun i -> Int i)) <|>
    (bool >|= (fun b -> Bool b)) <|>
    (keyword "Unit" >| Unit)

  let com : com parser =
    (ws >> (keyword "Push" >>= fun _ ->
             const >>= fun c ->
             pure (Push c))) <|>
    (ws >> keyword "Pop" >| Pop) <|>
    (ws >> keyword "Trace" >| Trace) <|>
    (ws >> keyword "Add" >| Add) <|>
    (ws >> keyword "Sub" >| Sub) <|>
    (ws >> keyword "Mul" >| Mul) <|>
    (ws >> keyword "Div" >| Div) <|>
    (ws >> keyword "And" >| And) <|>
    (ws >> keyword "Or" >| Or) <|>
    (ws >> keyword "Not" >| Not) <|>
    (ws >> keyword "Lt" >| Lt) <|>
    (ws >> keyword "Gt" >| Gt)

    
    let rec prog ls =
      match com ls with
      | Some (cmd, rest) ->
        (match char ';' rest with
        | Some (_, remaining) ->
          (match prog remaining with
          | Some (cmds, final) -> Some (cmd :: cmds, final)
          | None -> Some ([cmd], remaining))
        | None -> Some ([cmd], rest))
      | None -> None
    
    and prog_whitespace ls =
      match prog ls with
      | Some (cmds, ls') ->
        (match ws ls' with
        | Some (_, ls'') -> Some (cmds, ls'')
        | None -> Some (cmds, ls'))
      | None -> None

let interp (s : string) : string list option =
  match parse (ws >> prog_whitespace) s with
        | Some (p, []) ->
          let rec exec p stack trace =
            match p, stack with
            | [], _ -> Some ( trace)
            | Push c :: p, stack -> exec p (c :: stack) trace
            | Pop :: p, _ :: stack -> exec p stack trace
            | Pop :: p, [] -> return_panic p [] trace
            | Trace :: p, c :: stack ->
              let updated_trace = (toString c) :: trace in
              let updated_stack = Unit :: stack in
              exec p updated_stack updated_trace
            | Trace :: p, [] -> return_panic p [] trace
  
            | Add :: p, Int i1 :: Int i2 :: stack -> exec p (Int (i1 + i2) :: stack) trace
            | Add :: p, _ -> return_panic p stack trace
  
            | Sub :: p, Int i1 :: Int i2 :: stack -> exec p (Int (i1 - i2) :: stack) trace
            | Sub :: p, _ -> return_panic p stack trace
  
            | Mul :: p, Int i1 :: Int i2 :: stack -> exec p (Int (i1 * i2) :: stack) trace
            | Mul :: p, _ -> return_panic p stack trace
  
            | Div :: p, Int i1 :: Int i2 :: stack -> if i2 = 0 then return_panic p stack trace else exec p (Int (i1 / i2) :: stack) trace
            | Div :: p, _ -> return_panic p stack trace
  
            | And :: p, Bool b1 :: Bool b2 :: stack -> exec p (Bool (b1 && b2) :: stack) trace
            | And :: p, _ -> return_panic p stack trace
  
            | Or :: p, Bool b1 :: Bool b2 :: stack -> exec p (Bool (b1 || b2) :: stack) trace
            | Or :: p, _ -> return_panic p stack trace
  
            | Not :: p, Bool b :: stack -> exec p (Bool (not b) :: stack) trace
            | Not :: p, _ -> return_panic p stack trace
  
            | Lt :: p, Int i1 :: Int i2 :: stack -> exec p (Bool (i1 < i2) :: stack) trace
            | Lt :: p, _ -> return_panic p stack trace
            
            | Gt :: p, Int i1 :: Int i2 :: stack -> exec p (Bool (i1 > i2) :: stack) trace
            | Gt :: p, _ -> return_panic p stack trace
          and return_panic p stack trace = Some ( ("Panic" :: trace)) 
          in
          exec p [] []
        | _ -> None