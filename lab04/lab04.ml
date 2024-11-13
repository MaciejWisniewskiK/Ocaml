(* Task 1 *)
(* First greater than x *)

let rec first_greater x l = match l with
  | [] -> None
  | h::t -> if h > x then Some h else first_greater x t
;;

(* Task 2 *)
(* Modify plateau from previous lab to return None if list is empty *)
let plateau l = match List.fold_left
  (fun ak x -> match ak with
    | (cv, cl), max when cv == x -> (cv, cl + 1), max
    | (cv, cl), (_, ml) when cl > ml -> (x, 1), (cv, cl)
    | curr, max -> (x, 1), max)
  ((0, 0), (0, 0)) l with
  | (_, 0), (_, 0) -> None
  | (cv, cl), (mv, ml) when cl > ml -> Some (cv, cl)
  | _, max -> Some max
;;

(* Task 3 *)
(* Write a recursive function to map a sequence *) 
(* m : ('a -> 'b) -> 'a sequence -> 'b sequence *)

type 'a sequence = Empty | NonEmpty of 'a * 'a sequence;;
let rec m f s = match s with
  | Empty -> Empty
  | NonEmpty (h, t) -> NonEmpty (f h, m f t)
;;


(* Next tasks will operate on trees defined as follows: *)
type 'a tree = 
  | Node of 'a tree * 'a * 'a tree
  | Leaf;;

let rec size t = match t with
  | Node (l, _, r) -> size l + size r
  | Leaf -> 1
;;

(* Task 4 *)
(* Height of the tree *)

let rec height t = match t with
  | Node (l, _, r) -> 1 + max (height l) (height r)
  | Leaf -> 0
;;

(* Task 5 *)
(* Generate full binary tree. Values of the nodes should be set to depth. *)

let full_tree n = 
  let rec temp n d = match n with
    | 0 -> Leaf
    | _ -> Node (temp (n - 1) (d + 1), d, temp (n - 1) (d + 1))
  in temp n 0
;;

(* Task 6 *)
(* Read node values from a tree to a list *)
let read_tree t =
  let rec temp t ak = match t with
    | Node (l, v, r) -> temp l (v::(temp r ak))
    | Leaf -> ak
  in temp t []
;;

(* Task 7 *)
(* Diameter of a tree *)
let rec diameter t = match t with
  | Node (l, _, r) -> max (height l + height r + 1) (max (diameter l) (diameter r))
  | Leaf -> 0
;;

(* Task 8 *)
(* Check if a tree is ultraleft *)
let rec ultraleft t = match t with
  | Leaf -> true
  | Node (l, _, r) -> ultraleft l && ultraleft r && (size l >= size r)
;;

(* Task 9 *)
(* Remove some subtress so that the remaining tree has the greatest possible sum *)
let rec leaning t = match t with
  | Leaf -> Leaf
  | Node (l, v, r) -> 
    let rec sum t = match t with
      | Leaf -> 0
      | Node (l, v, r) -> v + sum l + sum r
    in 
    let t' = Node (leaning l, v, leaning r) in
    if sum t' > 0 then t' else Leaf
;;

(* Test for task 9 *)
(* Expected answer: int tree = Node (Node (Leaf, 5, Leaf), -2, Leaf) *)
leaning (Node (Node (Leaf, 5, Leaf), -2, Node (Node (Leaf, 5, Leaf), -6, Node (Leaf, -3, Leaf))));;
