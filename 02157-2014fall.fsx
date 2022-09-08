// Problem 1 (20%)
type Rel<'a,'b> = ('a * 'b list) list;;

let rel : Rel<int, string> = [(1, ["a"; "b"; "c"]); (4, ["b"; "e"])];;

// Point 1
let rec apply a = function
  | [] -> []
  | (x, xs) :: tail -> if x = a
                        then xs
                        else apply a tail;;

// Point 2
let rec inRelation x y = function
  | [] -> false
  | (xx, xs) :: tail -> if xx = x
                          then List.contains y xs
                          else inRelation x y tail;;

// Point 3
let rec insert x y = function
  | [] -> [(x, [y])]
  | (xx, xs) :: tail -> if xx = x
                          then (xx, y :: xs) :: tail
                          else (xx, xs) :: insert x y tail;;

// Point 4
let rec toRel ls =
  let rec aux rel = function
    | [] -> rel
    | (x, y) :: tail -> aux (insert x y rel) tail

  aux [] ls;;

// Problem 2 (25%)
// Point 1
let multTable n = Seq.init 10 (fun x -> (x+1)*n);;

// Point 2


// Point 3
let rec an n = 
  if n = 0
    then ""
    else "a" + an (n-1);;

let p3 = Seq.init 10 (fun x -> an (x + 1));;

let rec f i = function
  | [] -> []
  | x :: xs -> (x + i) :: f (i * i) xs;;

// Point 4
(*
  f takes in a list of integers and returns a list of increments of each list by a square of its negative index
*)

// Point 5
let rec f' acc i = function
  | [] -> List.rev acc
  | x :: xs -> f' ((x + i) :: acc) (i * 1) xs;;

let rec f'' cc acc i ls =
  if cc = 0
    then List.rev acc
    else match ls with 
          | [] -> failwith "Too many continuation calls"
          | x :: xs -> f'' (cc - 1) ((x + i) :: acc) (i * i) xs;;

// Problem 3 (20%)
type T<'a> = N of 'a * T<'a> list

let rec f (N (e, es)) = e :: g es

and  g = function
      | [] -> []
      | e :: es -> f e @ g es;;

let rec h p = function
  | N (e, _) when p e -> N (e, [])
  | N (e, es) -> N (e, List.map (h p) es);;

let rec k (N (_, es)) = 1 + List.fold max 0 (List.map k es);;

// Point 1
let t1 = N (1, []);;
let t2 = N (2, []);;
let t3 = N (3, [t1;t2]);;

// Point 2
(*
  f T<'a> -> 'a list
  f extracts all 'a values into a list

  g T<'a> list -> 'a list
  g applies f onto all values in T<'a> list

  h ('a -> bool) -> T<'a> -> T<'a>
  h returns a node in the tree t that matches p

  k T<'a> -> int
  k returns number of nodes in a tree
*)

// Problem 4
type  Outcome =
  | S
  | F

type Sample = Outcome list
type ProbTree =
  | Branch of string * float * ProbTree * ProbTree
  | Leaf of string

let exp  = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D"))

// Point 1
let rec probOK = function
  | Leaf _ -> true
  | Branch (_, x, b1, b2) -> (0.0 <= x && x <= 1.0) && probOK b1 && probOK b2;;

// Point 2
let rec isSample = function
  | ([], Leaf _) -> true
  | ([], Branch _) -> false
  | (x :: xs, Leaf _) -> false
  | (x :: xs, Branch (_, _, tl, tr)) -> match x with
                                          | S -> isSample (xs, tr)
                                          | F -> isSample (xs, tl);;
