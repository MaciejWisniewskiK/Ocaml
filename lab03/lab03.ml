(* Task 10 *)
(* Sum of sets represented as strictly increasing lists *)

let rec sumset l1 l2 = match l1, l2 with
  | l , [] | [], l -> l
  | h1 :: t1, h2 :: t2 when h1 == h2 -> h1 :: sumset t1 t2
  | h1 :: t1, h2 :: _ when h1 < h2 -> h1 :: sumset t1 l2
  | _, h2 :: t2 -> h2 :: sumset l1 t2
;;


(* Task 11 *)
(* Take every other element of a list recursively and using List.fold_left *)

let rec every_other_rec l = match l with
  | [] -> []
  | [x] -> [x]
  | h :: _ :: t -> h :: every_other_rec t
;;

let every_other_fold l = match List.fold_left 
    (fun ak x -> match ak with
      | true, l -> (false, x :: l)
      | false, l -> (true, l)) 
    (true, []) l with
  | _, l -> List.rev l
;;

(* When using fold_left elements come out in reversed order *)
(* We also have to remember to strip the temporary boolean from ak *)

(* Task 12 *)
(* Find the length and value of the longest plateau *)

(* ak = (current_value, current_len), (max_value, max_len) *)
let plateau l = match List.fold_left
    (fun ak x -> match ak with
      | (cv, cl), max when cv == x -> (cv, cl + 1), max
      | (cv, cl), (_, ml) when cl > ml -> (x, 1), (cv, cl)
      | curr, max -> (x, 1), max)
    ((0, 0), (0, 0)) l with
  | (_, 0), (_, 0) -> invalid_arg "empty list"
  | (cv, cl), (mv, ml) when cl > ml -> cv, cl
  | _, max -> max
;;


(* Task 13 *)

(* Insertion Sort *)
let rec insert x l = match l with
  | [] -> [x]
  | h :: t -> if x < h then x :: l else h :: insert x t
;;

let rec insertion_sort l = match l with
  | [] -> []
  | h :: t -> insert h (insertion_sort t)
;;

let insert_fold x l = match List.fold_left 
    (fun ak y -> match ak with
      | false, l when x < y -> (true, y :: x :: l)
      | b, l -> (b, y :: l)
    ) (false, []) l with
  | false, l -> List.rev (x :: l)
  | _, l -> List.rev l
;;

let insertion_sort_fold l = List.fold_left (fun ak x -> insert_fold x ak) [] l;;
(* It was much easier to use fold in sort function comparing to the insert *)

(* Task 14 *)
(* Apply a list of functions to an element *)

let rec apply lf x = match lf with
  | [] -> x
  | h :: t -> h (apply t x)
;;

let apply_fold lf x = List.fold_right (fun f x -> f x) lf x;;

(* Task 15 *)
(* Apply a list of functions to a list of elements (i-th function to i-th element) *)

let rec apply_list lf l = match lf, l with
  | [], _ | _, [] -> []
  | hf :: tf, h :: t -> hf h :: apply_list tf t
;;

let apply_list_fold lf l = List.rev (List.fold_left2 (fun ak f x -> f x :: ak) [] lf l);;

(* The function using fold throws an error when lists are of different length *)