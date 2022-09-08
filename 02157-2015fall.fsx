// Problem (30%)
type Appliance = string;;
type Usage = Appliance * int;;

let ad1 = ("washing machine", 2);;
let ad2 = ("coffee machine", 1);;
let ad3 = ("dishwasher", 2);;
let ats = [ad1; ad2; ad3; ad1; ad2];;

// Point 1
let rec inv = function
	|	[] -> true
	| (_, x) :: tail -> (x > 0) && inv tail;;

// Point 2
let rec durationOf a = function
	| [] -> 0
	| (x, y) :: tail when a = x -> y + durationOf a tail
	| _ :: tail -> durationOf a tail;;

// Point 3
let rec wellFormed ls =
	let rec durationOf = function
		| [] -> 0
		| (_, x) :: tail -> x + durationOf tail

	(inv ls) && (durationOf ls <= 24);;

// Point 4
let rec delete a = function
	| [] -> failwith "Can't delete a non-existing element"
	| (x, y) :: tail when x = a -> tail
	| x :: tail -> x :: delete a tail;;

type Price = int;;
type Tariff = Map<Appliance, Price>;;

// Point 5
let rec isDefined (trf:Tariff) = function
	| [] -> true
	| (x, _) :: tail -> match trf.TryFind x with
												| Some y -> isDefined trf tail
												| None -> false;;

// Point 6
let rec priceOf (trf:Tariff) = function
	| [] -> 0
	| (x, y) :: tail -> match trf.TryFind x with
												| Some z -> z * y + priceOf trf tail
												| None -> failwith "Can't find the item in the Tariff map"

// Problem 2 (35%)
let rec g1 p = function
	| x::xs when p x -> x :: g1 p xs
	|_ -> [];;

let rec g2 f h n x =
  match n with
  | _ when n<0 -> failwith "negative n is not allowed"
  | 0          -> x
  | n          -> g2 h f (n-1) (f x);;

// Point 1
(*
	g1 : ('a -> bool) -> 'a list -> 'a list

	g2 : ('a -> 'a) -> ('a -> 'a) -> int -> 'a -> 'a
*)

// Point 2
let rec g1' acc p = function
	| x :: xs when p x -> g1' (x :: acc) p xs
	| _ -> List.rev acc;;

// Problem 3 (35%)
type Name = string;;
type Flow = int;;
type River =
	R of Name * Flow * Tributaries

and Tributaries = River list;;

// Point 1
let riv1 = R ("R1", 5, []);;
let riv3 = R ("R3", 8, []);;
let riv4 = R ("R4", 2, []);;
let riv2 = R ("R2", 15, [riv4]);;
let riv = R ("R", 10, [riv1; riv2; riv3]);;

// Point 2
let rec contains n = function
	| R (x, _, []) -> x = n
	| R (x, y, head :: tail) -> x = n || (contains n head) || (contains n (R (x, y, tail)));;

// Point 3
let rec allNames = function
	| R (x, _, []) -> [x]
	| R (x, y, head :: tail) -> allNames head @ allNames (R (x, y, tail));;

// Point 4
let rec totalFlow = function
	| R (_, x, []) -> x
	| R (x, y, head :: tail) -> totalFlow head + totalFlow (R (x, y, tail));;

// Point 5
let rec mainSource riv =
	let rec aux = function
		| R (x, y, []) -> [(x, y)]
		| R (x, y, head :: tail) -> aux head @ aux (R (x, y, tail))

	let rec aux' = function
		| [] -> failwith "Empty list"
		| [(x, y)] -> (x, y)
		| (x,y) :: ((x1, y1) :: tail) -> if y < y1
																			then aux' ((x1, y1) :: tail)
																			else aux' ((x, y) :: tail)

	aux' (aux riv);;

let rec tryInsert n t = function
	| R (x, y, z) when x = n -> Some (R (x, y, z))
	| R (x, y, []) -> Some (R (x, y, [t]))

















