// Problem 1 (35%)

type Prize = string;;
type Achievement = int;;
type PrizeTable = (Prize * Achievement) list;;

let pt = [("p1", 3); ("p2", 5); ("p3", 8); ("p4", 11)];;

// Point 1
let rec inv (pt:PrizeTable) =
  match pt with
  | [] -> true
  | [x] -> true
  | (_, x) :: ((p, y) :: tail) -> (x < y) && inv ((p, y) :: tail);;
  
// Point 2
let rec prizesFor (a:Achievement) (pt:PrizeTable) =
  match pt with
  | [] -> []
  | (x, y) :: tail when y < a -> x :: prizesFor a tail
  | _ :: tail -> [];;

// Point 3
let rec increase k (pt:PrizeTable) =
  match pt with
  | [] -> []
  | [(x, y)] -> [(x, y + k)]
  | (x, y) :: tail -> (x, y + k) :: increase k tail;;

// Point 4
let rec add (p,a) (pt:PrizeTable) =
  match pt with
  | [] -> [(p,a)]
  | (p1, a1) :: tail when a < a1 -> (p, a) :: ((p1, a1) :: tail)
  | (p1, a1) :: tail -> (p1, a1) :: add (p,a) tail;;

// Point 5
let rec add' (p,a) (pt:PrizeTable) =
  match pt with
  | [] -> [(p,a)]
  | (p1, a1) :: tail when a = a1 -> failwith "TWO EQUAL VALUES!!!"
  | (p1, a1) :: tail when a < a1 -> (p, a) :: ((p1, a1) :: tail)
  | (p1, a1) :: tail -> (p1, a1) :: add (p,a) tail;;

let merge (pt1:PrizeTable) (pt2:PrizeTable) =
  let rec aux' acc (pt:PrizeTable) =
    match pt with
    | [] -> acc
    | [x] -> aux' (add' (x) acc) []
    | x :: tail -> aux' (add' (x) acc) tail

  aux' pt1 pt2;;

// Point 6
let prizesFor' a list = 
  List.filter (fun (_, x) -> x < a) list;;

let increase' k (list: ('a * int) list) = 
  List.map (fun (x,y) -> (x, y + k)) list;;

let merge' (ls1:PrizeTable) (ls2:PrizeTable) = 
  let temp = 
    ls1
    |> List.fold (fun acc x -> add' x acc) []
  
  ls2
  |> List.fold (fun acc x -> add' x acc) temp;;

// Problem 2 (20%)
let rec choose f = function
  | [] -> []
  | x :: rest -> 
    match f x with
    | None -> choose f rest
    | Some y -> y :: choose f rest;;

(*
  choose has 2 arguments f and xs so you can generalize the types into arg1 -> arg2 -> result
  From pattern matching we can see that xs is a list of some 'a, meaning arg2 is 'a list
  From the second pattern matching we can see f takes in some value of 'a and gives a new option value of some 'b, meaning arg1 is ('a -> 'b option)

  Therefore, the very result is a list some values 'b, meaning result is 'b list
*)

let chEven n =
  if n%2 = 0
    then Some (string n)
    else None;;

// Point 2
(*
  choose chEven [1;2;3;4;5] = choose chEven [2;3;4;5] = ["2"] :: choose chEven [3;4;5] = ["2"] :: choose chEven [4;5] = ["2"; "4"] :: choose chEven [5] = ["2"; "4"]
*)

// Point 3
let rec choose' f acc = function
  | [] -> List.rev acc
  | x :: rest -> 
    match f x with
    | None -> choose' f  acc rest
    | Some y -> choose' f  (y :: acc) rest;;
                  
// Point 4
// Continuation based is literally cursed!!!

// Problem 3 (15%)
type T =
  | One of int
  | Two of int * T * int * T;;

let rec f p (t:T) =
  match t with
  | One v when p v -> [v]
  | Two (v1, t1, _, _) when p v1 -> v1 :: f p t1
  | Two (_, _, v2, t2) -> v2 :: f p t2
  | _ -> [];;

// Point 1
(*
  f takes in two arguments a function and a node of type T
  the function p takes in an int and returns a boolean
  The result of the function f is a list of integers
  (int -> bool) -> T -> int list
*)

// Point 2
let p x = x > 0;;
let tt1 = One 1
let tt2 = Two (2, tt1, 3, tt1);;
let tt3 = Two (-2, tt1, 4, tt1);;
let tt4 = One 0;;

// Problem 4 (30%)
type Trie<'a> =
  N of 'a * bool * Children<'a>

and Children<'a> =
      Trie<'a> list;;

let t1 = N (0, false, [N (0, false, [N (1, true, [])])]);;
let t2 = N (0, true, [N (0, false, [N (1, true, [])])]);;

let ta = N (1, true, [N (2, true, [])]);;
let tb = N (3, false, [N (0, true, [])]);;
let tc = N (2, true, []);;

let t3 = N (0, false, [ta; tb; tc]);;

// Point 1
let rec countNodes (t:Trie<'a>) =
  match t with
  | N (_, _, []) -> 1
  | N (x, y, head :: tail) -> countNodes head + countNodes (N (x, y, tail));;

// Point 2
type Word<'a> = 'a list;;

let rec accept (w:Word<'a>) (t:Trie<'a>) =
  match (w,t) with
  | ([], N (_, _, _)) -> true
  | ([head], N (x, y, _)) when x = head && y -> true
  | (head :: tail, N (x, y, [])) -> false
  | (head :: tail, N (x, y, z :: zs)) when x = head -> 
      accept tail z || accept (head :: tail) (N (x, y, zs))
  | _ -> false;;

// Point 3
let rec wordsOf (t:Trie<'a>) :Set<Word<'a>> =
  let rec aux (acc:Word<'a>) (t:Trie<'a>) =
    match t with
    | N (x, y, ls) when y -> ((x :: acc) |> List.rev) :: aux acc (N (x, not y, ls))
    | N (x, y, []) -> []
    | N (x, y, head :: tail) -> (aux (x :: acc) head) @ aux acc (N (x, y, tail))
  
  aux [] t |> Set.ofList;;

// Point 4
let rec uselessLeaves (t:Trie<'a>) =
  match t with
  | N (_, x, []) when x -> false
  | N (_, _, []) -> true
  | N (x, y, [xs]) -> uselessLeaves xs
  | N (x, y, head :: tail) -> uselessLeaves head || uselessLeaves (N (x, y, tail));;

// Point 5
let max x y =
  if x < y
    then y
    else x;;

let rec degree (t:Trie<'a>) =
  match t with
  | N (_, _, []) -> 0
  | N (x, y, head :: tail) -> 
    max ((head :: tail) |> List.length) (max (degree head) (degree (N (x, y, tail))));;