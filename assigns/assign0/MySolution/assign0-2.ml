#use "./../assign0.ml";;

let rec 
isPrime(n0: int): bool = 
  if n0 <= 1 then
    false
  else
    let rec divisible(s: int) = 
      if s * s > n0 then
        true
      else if n0 mod s = 0 then
        false
      else
        divisible (s + 1)
    in
    divisible 2
;;
