// Problem 1 30%
type name = string
type phone = int
type level = int

type description = phone * level
type register = (name * description) list

// Point 1
let Joe = ("Joe", (10101010, 4))
let Sal = ("Sal", (11111111, 2))
let Sam = ("Sam", (12121212, 7))
let Jane = ("Jane", (13131313, 1))

let register: register = [ Joe; Sal; Sam; Jane ]

// Point 2
let getPhone (name: name) (register: register) =
    List.sumBy (fun (n, (x, _)) -> if n = name then x else 0) register

// Point 3
let delete (name: name, register: register) = List.filter (fst >> (<>) name) register

// Point 4s
let getCandidates (level: level) (registers: register) =
    List.collect (fun (a, (b, c)) -> if abs (c - level) < 3 then [ (a, b) ] else []) register
// Problem 2 30%
type exp =
    | C of int
    | BinOp of exp * string * exp

// Point 1
let exp1 = C 1
let exp2 = C 2
let exp3 = BinOp(exp1, "+", exp2)
let exp4 = BinOp(exp2, "*", exp3)

// Point 2
let rec toString (exp: exp) =
    match exp with
    | C x -> string x
    | BinOp (a, b, c) -> "(" + toString a + b + toString c + ")"

// Point 3
let rec extract' (exp: exp) =
    match exp with
    | C _ -> []
    | BinOp (a, b, c) -> b :: extract' a @ extract' c

let extract (exp: exp) = extract' exp |> Set.ofList

// Point 4
type expr =
    | C of int
    | BinOp of expr * string * expr
    | Id of string
    | Def of string * expr * expr

let rec isDef (expr: expr) =
    let rec aux (id: string) (expr: expr) =
        match expr with
        | C _ -> true
        | BinOp (x, _, y) -> aux id x && aux id y
        | Id x -> x = id
        | Def (x, y, z) -> aux x y && aux x z

    aux "" expr

// Problem 3 20%
type 'a tree =
    | Lf
    | Br of 'a * 'a tree * 'a tree

let rec f (n, t) =
    match t with
    | Lf -> Lf
    | Br (a, t1, t2) -> if n > 0 then Br(a, f (n - 1, t1), f (n - 1, t2)) else Lf

let rec g p =
    function
    | Br (a, t1, t2) when p = a -> Br(a, g p t1, g p t2)
    | _ -> Lf

let rec h k =
    function
    | Lf -> Lf
    | Br (a, t1, t2) -> Br(k a, h k t1, h k t2)

// Point 1
(*
  Function f's type is defined by:
  int * 'a tree -> 'a tree
  It basically takes in a tree A and returns a new tree B of n levels of A.
*)
(*
  Function g's type is defined by:
  'a -> 'a tree -> 'a tree
  It basically takes in a tree A and returns a new tree B where all branches with the value not p are cut and replaced by leafs.
*)
(*
  Function h's type is defined by:
  ('a -> 'b) -> 'a tree -> 'b tree
  It basically takes in a tree A and returns a new tree B where all keys are mapped along k.
*)

// Problem 4 20%
let rec map f =
    function
    | [] -> []
    | x :: xs -> f x :: map f xs

let rec rev =
    function
    | [] -> []
    | x :: xs -> rev xs @ [ x ]

(*
  Function map maps f onto all elements of the list, but does not change the order of elements in the list
  Function rev reverses the order of elemnts in the list, but does not change the value of elements in the list

  Therefore, if you map a function on a reversed list, it is the same as reversing a mapped list.
*)
