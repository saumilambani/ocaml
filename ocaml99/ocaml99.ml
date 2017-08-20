(** OCAML 99 Problems **)

let li = [1;2;3;4];;

(* 1 - last element*)
let rec last = function
| [] -> None 
| h::[] -> Some h
| _::t -> last t  

let la = last li;;

match la with 
| None -> Printf.printf("None ")
| Some x -> Printf.printf("%n\n") x 

(* 2 -last two *)
let rec lastTwo = function 
| [] -> None 
| _::[] -> None
| x::y::[] -> Some (x::y::[])
| _::_::t -> lastTwo t

let la2 = lastTwo li;;

match la2 with 
| None -> Printf.printf("None ")
| Some (x::y::[]) -> Printf.printf("%n %n\n") x y
| _ -> Printf.printf(" Error")

(* 3 -kth element*)
let rec kth k = function
| [] -> None
| h::t -> if k = 1 then 
            Some h 
          else 
            kth (k - 1) t

let lak = kth 3 li;;

match lak with 
| None -> Printf.printf("None")
| Some x -> Printf.printf("%n\n") x

let x = List.nth li 3;;
Printf.printf("%n \n") x;;

(* 4 -Number of elements *)
let rec numElems acc = function
 | [] -> acc 
 | h::t -> numElems (acc + 1) t

let x = numElems 0 li;;
Printf.printf("%n \n") x;;

(* 5 - reverse a list *)
let rec rev acc = function
 | [] -> acc
 | h::t -> rev (h::acc) t

let () = List.iter (Printf.printf("%n ")) (rev [] li);; 

(* 6 - check if string is plaindrome *)
let isPalindrom list = 
   list = List.rev list

let a = ["a"; "b"; "c"; "b"; "a"];;
let b = rev [] a;;
let () = List.iter (Printf.printf("%s ")) a;; 
let () = List.iter (Printf.printf("%s ")) b;; 

if (a = b) then 
   Printf.printf(" is palindrome \n")
else
   Printf.printf(" not palindrome \n")

let c = isPalindrom a;;

match c with 
| true -> Printf.printf ("is palindrom \n")
| false -> Printf.printf ("is not palindrom \n")

(* 7 - Flattening a nested list *)

type 'a node = 
| One of 'a
| Many of 'a node list;;

let flatten list = 
   let rec aux acc = function
   | [] -> acc
   | One x::t -> aux (x::acc) t 
   | Many l::t -> aux (aux acc l) t
   in
   List.rev (aux [] list);;

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

(* 8 - Eliminate consecutive duplicated of list elements *)
let rec compress acc last = function
| [] -> acc 
| h::t -> if (h = last) then 
            compress acc h t 
          else
             compress (h::acc) h t;;
let compressed = compress [] "" ["a";"a";"a";"b";"b";"c"];;
let () = List.iter (Printf.printf("%s ")) (List.rev compressed);; 

(* Alternate *)
let rec compress = function 
| a::(b::_ as t) -> if a = b then compress t else a :: compress t
| smaller -> smaller

