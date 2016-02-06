(* 
	Jonathan Kosgei
	https://www.github.com/jonathankosgei/kalenjinCamel
	https://www.facebook.com/jonathankbt71
	
	Ocaml list operations functions.
*)

(* Recursively calculate the length of a list. Polymorphous. *)

let rec length l =
	match l with 
	[] -> 0
	|_::t -> 1 + length t;;
	
	

