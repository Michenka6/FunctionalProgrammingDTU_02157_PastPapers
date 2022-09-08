// Problem 1 (30%)
type Multiset<'a when 'a : equality> = ('a * int) list;;

let example : Multiset<string> = [("b", 3); ("a", 5); ("d", 1)];;
 
 // Point 1
 let rec inv = function
 	| [] | [_] -> true
 	| (x, _) :: ((y, z) :: tail) -> not (x = y) && inv ((y, z) :: tail);;

// Point 2
let rec insert a n = function
	| [] -> [(a, n)]
	| (x, y) :: tail when a = x -> (x, (y+n)) :: tail
	| head :: tail -> head :: insert a n tail;;

// Point 3
// 'a -> Multiset<'a> -> int when 'a equality
let rec numberOf e = function
	| [] -> failwith "Could not find requested element!"
	| (x, y) :: tail when x = e -> y
	| _ :: tail -> numberOf e tail;;

// Point 4
let rec delete e = function
	| [] -> failwith "Could not find requested element!"
	| (x, y) :: tail when x = e -> (x, (y - 1)) :: tail
	| head :: tail -> head :: delete e tail;;

// Point 5
let rec union ((ms1: Multiset<'a>), (ms2: Multiset<'a>)) =
	match ms2 with
		| [] -> ms1
		| (x, y) :: tail -> union ((insert x y ms1), tail);;

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;
let exampleMap : MultisetMap<string>= 
	Map.empty
		|> Map.add "b" 3
		|> Map.add "a" 5 
		|> Map.add "d" 1;;

// Point 6
let inv' (msm : MultisetMap<string>) =
	msm |> Map.forall (fun x y -> y > 0);;

let insert' a n (msm : MultisetMap<string>) =
	match (msm |> Map.tryFind a) with
		| Some x -> (msm |> Map.add a (x + n))
		| None -> (msm |> Map.add a n);;

let rec union' (msm1 : MultisetMap<string>) (msm2 : MultisetMap<string>) =
	match (msm1 |> Map.toList) with
		| [] -> msm2
		| (x, y) :: tail -> union' (tail |> Map.ofList) (msm2 |> insert' x y);;

// Problem 2 (30%)
let rec f i = function
	| [] -> []
	| x :: xs -> (i, x) :: f (i * i) xs;;

type 'a Tree =
	| Lf
	| Br of 'a Tree * 'a * 'a Tree;;

let rec g p = function
	| Lf -> None
	| Br (_, a, t) when p a -> Some t
	| Br (t1, a, t2) -> match g p t1 with