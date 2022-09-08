// Problem 1 (35%)

type Prize = string;;
type Achievement = int;;
type PrizeTable = (Prize * Achievement) list;;

let pt = [("p1", 3); ("p2", 5); ("p3", 8); ("p4", 11)];;

// Point 1
let rec inv = function
  | [] -> true
  | [x] -> true
  | (_, x) :: ((p, y) :: tail) -> (x < y) && inv ((p, y) :: tail);;
  
// Point 2
let rec prizesFor a = function
  | [] -> []
  | (x, y) :: tail when y < a -> x :: prizesFor a tail
  | _ :: tail -> [];;

// Point 3
let rec increase k = function
  | [] -> []
  | [(x, y)] -> [(x, y + k)]
  | (x, y) :: tail -> (x, y + k) :: increase k tail;;

// Point 4
let rec add (p,a) = function
  | [] -> [(p,a)]
  | (p1, a1) :: tail when a < a1 -> (p, a) :: ((p1, a1) :: tail)
  | (p1, a1) :: tail -> (p1, a1) :: add (p,a) tail;;

// Point 5
let rec add' (p,a) = function
    | [] -> [(p,a)]
    | (p1, a1) :: tail when a = a1 -> failwith "TWO EQUAL VALUES!!!"
    | (p1, a1) :: tail when a < a1 -> (p, a) :: ((p1, a1) :: tail)
    | (p1, a1) :: tail -> (p1, a1) :: add (p,a) tail;;

let merge pt1 pt2 =
  let rec aux' acc = function
    | [] -> acc
    | [x] -> aux' (add' (x) acc) []
    | x :: tail -> aux' (add' (x) acc) tail

  aux' pt1 pt2;;

// Point 6
let prizesFor' a list = List.filter (fun (_, x) -> x < a) list;;

let increase' k list = List.map (fun (x,y) -> (x, y + k)) list;;

let merge' ls1 ls2 = 
  let temp = List.fold (fun acc x -> add' x acc) [] ls1
  List.fold (fun acc x -> add' x acc) temp ls2;;

// Problem 2 (20%)
let rec choose f = function
  | [] -> []
  | x :: rest -> match f x with
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
  | x :: rest -> match f x with
                                | None -> choose' f  acc rest
                                | Some y -> choose' f  (y :: acc) rest;;
                  

// Point 4
// let rec choose'' f i acc xs =
//   match i with
//     | 0 -> List.rev acc
//     | _ -> match f (List.head xs)
//             | None -> choose'' f (i - 1) acc (List.tail xs)
//             | Some y -> choose'' f (i - 1) (y :: acc) (List.tail xs);;

// Problem 3 (15%)
type T =
  | One of int
  | Two of int * T * int * T;;

let rec f p = function
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
let countNodes t =
  let rec aux = function
    | [] -> 0
    | N (_, _, []) :: tail -> 1 + aux tail
    | N (_, _, x :: xs) :: tail -> 1 + aux ([x]) + aux xs + aux tail

  aux [t];;

// Point 2
let accept w t =
  let rec aux (N (a, b, c)) = function
    | [] -> true
    | [x] -> a = x
    | x :: xs -> a = x && aux' xs c

  and aux' w = function
    | [] -> false
    | [x] -> aux x w
    | x :: xs -> aux x w || aux' w xs

  aux t w;;

// Point 3