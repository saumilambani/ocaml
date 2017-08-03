(* Newton Raphson method *)

let eps = 0.001 in

let rec newton_raphson x a =  
   let improv x a = 
      (x +. a/.x)/.2.
   in 

   let satis x a = 
      if (abs_float(x*.x -. a) < eps) then 
         true
      else 
         false
   in

   if satis x a then
      let () = Printf.printf "Sq %f = %f\n" a x in
      x
   else 
      newton_raphson (improv x a) a 
in

(* Assuming initial guess to be the same as the number *)
let square_root x = 
   newton_raphson x x
in

square_root 5.0;;
