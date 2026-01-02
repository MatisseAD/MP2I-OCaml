type intlist = 
| Nil
| Cons of int * intlist

let rec length_intlist = function
| Nil -> 0
| Cons (_, t) -> 1 + length_intlist t 

type stringList = 
| Nil
| Cons of string * stringList

let rec length_stringlist = function
| Nil -> 0
| Cons (_,t) -> 1 + length_stringlist t 

type 'a mylist =
  | []
  | (::) of 'a * 'a mylist

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t

