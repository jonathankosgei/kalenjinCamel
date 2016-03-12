(* 
	Jonathan Kosgei
	https://www.github.com/jonathankosgei/kalenjinCamel
	https://www.facebook.com/jonathankbt71
	
	Ocaml list operations functions.
*)

(* Sample OCaml list *)
let l = [23; 34; 65; 76; 45; 76; 1; 0; 344; 754];;
let empl = [];;


(* Recursively calculate the length of a list. Polymorphous. *)

let rec length l =
	match l with 
	[] -> 0
	|_::t -> 1 + length t;;
	
	
(*Take n elements from a list *)
let rec take n l = if n < 0 then raise (Invalid_argument "negative n") else
  match l with
  | [] -> raise (Invalid_argument "short list")
  | [a] -> [a]
  | h::t -> if n > 0 then h:: take (n-1) t else [];;




(*Drop n elements from a list *)

