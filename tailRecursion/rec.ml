(* Calculating factorial using tail and non-tail recursion *)

(*** FACTORIAL ***)
(* NON TAIL RECURSION *)
let rec fact i =
   if i = 0 then
      1
   else
      i*(fact (i - 1))
in
Printf.printf "%n \n" (fact 5);;


(* TAIL RECURSION *)
let fact2 i = 
  let rec loop acc i = 
    if i = 0 then
      acc
   else
      loop (acc*i) (i-1)
  in
  loop 1 i
in
let k = fact2 5 in
Printf.printf "%n \n" k;;

(** Reversing a list(l) **)
let l = [1;2;3;4];;

let rec rev acc = function
| [] -> acc 
| h::t -> rev (h::acc) t

let a = rev [] l;; 
let () = List.iter (Printf.printf "%n ") a;;

(** Applying a function(f) to a list(l) **)
let f a  = 2*a;;

let rec rev_map f acc = function
| [] -> acc 
| h::t -> rev_map f (f h::acc) t

let l_f = rev [] (rev_map f [] l);; 
let () = List.iter (Printf.printf "%n ") l_f;;
