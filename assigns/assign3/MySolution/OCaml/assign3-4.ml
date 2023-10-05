#use " ./../../assign3.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_of_buddies(word: string): string list = 
  let len = string_length word
 in
  let buddies = ref []
 in
  for i = 0 to len - 1 do
    let l = if i = 0 then "" else String.sub word 0 i
   in
    let r = if i = len - 1 then "" else String.sub word (i + 1) (len - i -1)
   in
    for c = ord 'a' to ord 'z' do
      let next = l ^ Char.escaped (chr c) ^ r
     in
      if next <> word then buddies := next:: !buddies
    done
  done; !buddies
  ;;