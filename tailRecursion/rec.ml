(* Calculating factorial using tail and non-tail recursion *)

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
