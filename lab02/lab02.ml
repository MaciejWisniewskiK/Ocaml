(* Task 1 *)
(* examples of lists of lists*)
[[];[]];;
[[[]]];;
[[]; [[]]];;

(* Task 2 *)
(* Take second element of the first list and first element of the second list *)
let sec_fi_and_fi_sec l = match l with
    | (_ :: p :: _) :: (d :: _) :: _ -> p + d
    | _ -> invalid_arg "za krótka lista"
;;

(* Task 3 *)
(* get length of a list *)
let len1 l = match l with
    | [] -> 0
    | _ :: t -> 1 + len1 t
;;

let len2 l = 
    let pom ak l = match l with
        | [] -> ak
        | _ :: t -> pom (ak + 1) t
    in pom 0 l
;;

(* generate list of natural numbers up to n *)
let rec rosnace1 n =
    let rec pom ak n =
        if n = 0 then ak else pom (n :: ak) (n - 1)
    in
    pom [] n
;;

let rec rosnace2 n =
    let rec pom s e =
        if s > e then [] else s :: pom (s + 1) e
    in
    pom 1 n
;;

(* Task 4 *)
(* reverse a list *)
let rev l =
    let rec pom ak l = match l with
        | [] -> ak
        | h :: t -> pom (h :: ak) t
    in
    pom [] l
;;


(* Task 5 *)
(* Take a list without the last element *)
let bez_ost l =
    let rec pom ak l = match l with
        | [] -> ak
        | _ :: [] -> ak
        | h :: t -> pom (h :: ak) t
    in
    rev (pom [] l)
;;

(* Generate a list of sufixes *)
let rec ogony l = match l with
  | [] -> [[]]
  | _ :: t -> l :: ogony t
;;

(* Generate a list of prefixes *)
let rec poczatki l = match l with
    | [] -> [[]]
    | _ -> l :: poczatki (bez_ost l)
;;

(* Task 6 *)
let rec map f l = match l with
| [] -> []
| h :: t -> f h :: map f t
;;

(* a function of type 'a -> 'b -> 'a list -> 'b list *)
let f a b l = match l with
    | [] -> []
    | h :: _ -> if a = h then [b] else [b; b]
;;

(* Task 7 *)
(* reverse all sublists *)
let odwroc_wszystkie l = 
    map rev l
;;

(* Task 8 *)
(* get only the even numbers *)
let parzyste l = 
    List.filter (fun x -> if x mod 2 == 0 then true else false) l
;;

(* Task 9 *)
(* get only the even numbers and divide by 2 *)
let polowki l =
    map (fun x -> x/2) (parzyste l)
;;
