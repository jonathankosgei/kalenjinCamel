(* recursively replace ! with . *)
let rec calm (lst:char list) : char list =
    match lst with 
    | h::t -> if h = '!' then '.'::calm t else h :: calm t
    | _ -> [];;
    
(* clips integers less than 1 to 1 and greater than 10 to 10 *)
let rec clip (lst:int list) : int list =
    match lst with
    | h::t -> 
      if h < 1 then 1::clip t else if h > 10 then 10::clip t else h::clip t
    | [] -> [];;

let clip1 (integ:int ) : int =
    if integ < 1 then 1 else if integ > 10 then 10 else integ;;
    
let clip_list (lst:int list) : int list = List.map clip1 lst;;

let clip_list2 (lst:int list) : int list =
    List.map (fun x -> if x < 1 then 1 else if x > 10 then 10 else x) lst;;

(* applies a function x times to an argument *)
let rec apply (f:'a -> 'b) (times:int) (argue: 'a) : ('a -> 'b) =
    if times < 1 then f else apply f (times - 1) (f argue);;
    
(* insert taking a function *)
let rec insert (f:'a-> 'b -> bool) (el:int) (lst:int list) : int list =
    match lst with
    | h::t -> if f el h then el::lst else h::insert f el t
    | [] -> [];;

(* filters elements based on the logical check of the function *)
let rec filter (f:'a -> bool) (lst:'a list) : 'a list =
    match lst with
    | h::t -> if f h then h::filter f t else filter f t
    | [] -> [];;

(* is true if all elements return true after function application *)
let rec for_all (f:'a -> bool) (lst: 'a list) : bool =
    match lst with
    | h::t -> if f h then for_all f t else false
    | [] -> true;;

(* maps through list of lists *)
let rec mapl (f: 'a list -> 'b list) (lst: 'a list list ) : 'b list list =
    List.map f lst;;

(* gives the smallest positive number from a list else throws an exception  *)
let smallest (lst:int list) :int =
    let rec small_inner (l:int list) (track: int) : int =
        match l with
        | h::t -> if h < 0 then small_inner t track else small_inner t h
        | [] -> if track < 0 then raise Not_found else track
    in
    small_inner lst (-1);; 
let smallest_or_zero (lst:int list) : int =
    try smallest lst with Not_found -> 0;;

(* calculate largest integer smaller than or equal to the sqrt of given int *)
exception Negative_int of string;;
let smint (integ:int): int =
    if integ < 0 then raise (Negative_int "Expected a positive integer") else
    let sqrt_float = sqrt (float integ) in
    int_of_float sqrt_float;;
let smart_smint (integ: int ) :int =
    try smint integ with Negative_int _ -> 0;;

(* determines the number of different keys in a dictionary *)
let keys (dictionary: (int * int) list) : int =
    let rec inner_keys (lst: (int*int) list) (prev_key: int) (k_num: int): int =
        match lst with
        | [] -> k_num - 1
        | (k,v)::t -> if k = prev_key then inner_keys t prev_key k_num else
            inner_keys t k (k_num + 1)
    in inner_keys dictionary min_int 0;;

(* replaces a key with a value, raises Not_found if key wasn't found *)
let rec replace (kv: (int * int)) (dict: (int * int) list) : (int * int) list =
    let k',v' = kv in
    match dict with
    | [] -> raise Not_found
    | (k,v)::t -> if k = k' then (k,v')::t else (k,v)::replace kv t ;;

(* builds dictionary from a list of keys and a list of values *)
let rec dict_build (keys:'a list) (values:'b list) : ('a*'b) list =
    if List.length keys <> List.length values then
    raise (Invalid_argument "Unmatching number of keys and values") else
    match keys with
    | [] -> []
    | h::t -> match values with
        | hd::tl -> (h,hd)::dict_build t tl
        | [] -> raise (Invalid_argument "Less keys");;
         
(* inverse gives a list of keys and a list of values from a dictionary *)
let inverse (dict:('a*'b) list): 'a list * 'b list =
    let rec inner_inverse 
    (dct:('a * 'b) list) (fin_list:'a list * 'b list): 'a list * 'b list =
        match dct with
        | [] -> ([],[])
        | (k,v)::t -> match fin_list with
            | klst,vlst -> (k::klst, v::vlst)
    in inner_inverse dict ([],[]);;
        
(* turns a list of pairs into a dictionary *)
let pairs_to_dict (lst:('a * 'b) list) : ('a * 'b) list =
    let rec inner_pair (lst': ('a * 'b) list) (prev:('k*'v)) : ('a*'b) list =
    match prev with k , v ->
    match lst' with
    | [] -> []
    | (aa,bb)::t -> if aa = k then (k,v)::inner_pair t (aa,bb) else
        (aa,bb)::inner_pair t (aa,bb)
    in 
    match lst with
    | [] -> raise (Invalid_argument "Empty list")
    | (a,b)::tail -> (a,b)::inner_pair tail (a,b);;

(* applies a function to elements in lists in a list *)
let mapl f lstlst = List.map (List.map f) lstlst;;

(* truncates lists to a specific length in a list of lists *)
let rec termnator count lst = (if count > 0 then 
    match lst with 
    | [] -> [] 
    | h::t -> h::termnator (count - 1) t
    else []) ;;
let truncate trunc lstlst = List.map (termnator trunc) lstlst;;

(* returns a list containing the first elements of llists in a list *)
let first_elem_lst_lst = List.map 
    (fun a -> match a with 
    | [] -> raise (Invalid_argument "Empty list element") 
    | h::_ -> h);;

(* type for representing rectangles*)
type rect = Rectangle of int * int | Square of int;;

(* calculate the area of a given rectangle *)
let rect_area r =
    match r with
    | Rectangle (a,b) -> a * b
    | Square c -> c * c;;

(* rotates a rectangle to be as tall as it is wide *)
let rect_rotate r =
    match r with 
    | Rectangle (l,w) -> if l > w then Rectangle (w,l) else r
    | _ -> r;;

(* returns a list of rotated rectangles in order from narrowest to widest *)
let rect_rotate_sort rects = 
    let rotated = List.map rect_rotate rects in
    let comparison r r' = 
        let integ a b = if a < b then -1 else if b > 1 then 1 else 0 in
        match r,r' with
        | Rectangle (l,_), Rectangle (l',_) -> integ l l'
        | Rectangle (l,_), Square w -> integ l w
        | Square w, Rectangle (l,_) -> integ w l
        | Square w, Square w' -> integ w w'
    in List.sort comparison rotated;;

(* return options to avoid raising exceptions when division by zero happens *)
let safe_divide a b = try Some (a/b) with Division_by_zero -> None;;
