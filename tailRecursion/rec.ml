(* Calculating factorial using tail and non-tail recursion *)

(*** FACTORIAL ***)
(* Tail Recursion *)
let rec fact i =
   if i = 0 then
      1
   else
      i*(fact (i - 1))
in
   
Printf.printf "%n \n" (fact 5);;


(* Non tail recursion *)
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

(** REVERSING A LIST **)
let rec rev acc = function
  [] -> acc 
| h::t -> rev (h::acc) t


let a = rev [] [1;2;3;4];; 
let () = List.iter (Printf.printf "%n ") a;;
