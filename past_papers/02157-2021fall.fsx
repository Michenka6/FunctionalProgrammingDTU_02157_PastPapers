// Problem 1 (35%)
type Prize = string
type Achievement = int
type PrizeTable = (Prize * Achievement) list

let pt: PrizeTable = [ ("p1", 3); ("p2", 5); ("p3", 8); ("p4", 11) ]

// Point 1
let rec inv (pt: PrizeTable) =
    match pt with
    | (_, a1) :: ((p, a2) :: tail) -> (a1 < a2) && inv ((p, a2) :: tail)
    | _ -> true

// Point 2
let rec prizesFor (a: Achievement) (pt: PrizeTable) =
    match pt with
    | (p1, a1) :: tail when a1 <= a -> p1 :: prizesFor a tail
    | _ -> []

// Point 3
let rec increase k (pt: PrizeTable) =
    match pt with
    | [] -> []
    | (p, a) :: tail -> (p, a + k) :: increase k tail

// Point 4
let rec add (p, a) (pt: PrizeTable) =
    match pt with
    | [] -> [ (p, a) ]
    | (p1, a1) :: tail when a < a1 -> (p, a) :: ((p1, a1) :: tail)
    | (p1, a1) :: tail -> (p1, a1) :: add (p, a) tail

// Point 5
let rec merge (pt1: PrizeTable) (pt2: PrizeTable) =
    match (pt1, pt2) with
    | [], [] -> []
    | [], x -> x
    | x, [] -> x
    | (p1, a1) :: xs, (p2, a2) :: ys when a1 = a2 -> failwith "error two equal values"
    | (p1, a1) :: xs, (p2, a2) :: ys when a1 < a2 -> (p1, a1) :: merge xs ((p2, a2) :: ys)
    | (p1, a1) :: xs, (p2, a2) :: ys -> (p2, a2) :: merge ((p1, a1) :: xs) ys

// Point 6
let prizesFor' a = List.filter (snd >> (<) a)

let increase' k = List.map (snd >> (+) k)

let merge' pt1 pt2 =
    List.fold (fun ls x -> add x ls) pt1 pt2

// Problem 2 (20%)
let rec choose f =
    function
    | [] -> []
    | x :: rest ->
        match f x with
        | None -> choose f rest
        | Some y -> y :: choose f rest

(*
  choose has 2 arguments f and xs so you can generalize the types into arg1 -> arg2 -> result
  From pattern matching we can see that xs is a list of some 'a, meaning arg2 is 'a list
  From the second pattern matching we can see f takes in some value of 'a and gives a new option value of some 'b, meaning arg1 is ('a -> 'b option)

  Therefore, the very result is a list some values 'b, meaning result is 'b list
*)

let chEven n =
    if n % 2 = 0 then Some(string n) else None

// Point 2
(*
  choose chEven [1;2;3;4;5] = choose chEven [2;3;4;5] = ["2"] :: choose chEven [3;4;5] = ["2"] :: choose chEven [4;5] = ["2"; "4"] :: choose chEven [5] = ["2"; "4"]
*)

// Point 3
let rec choose' f acc =
    function
    | [] -> List.rev acc
    | x :: rest ->
        match f x with
        | None -> choose' f acc rest
        | Some y -> choose' f (y :: acc) rest

// Point 4
let rec choose'' f k =
    function
    | [] -> k []
    | x :: rest ->
        match f x with
        | None -> choose'' f k rest
        | Some y -> choose'' f (fun x -> k (y :: x)) rest

// Problem 3 (15%)
type T =
    | One of int
    | Two of int * T * int * T

let rec f p (t: T) =
    match t with
    | One v when p v -> [ v ]
    | Two (v1, t1, _, _) when p v1 -> v1 :: f p t1
    | Two (_, _, v2, t2) -> v2 :: f p t2
    | _ -> []

// Point 1
(*
  f takes in two arguments a function and a node of type T
  the function p takes in an int and returns a boolean
  The result of the function f is a list of integers
  (int -> bool) -> T -> int list
*)

// Point 2
let p x = x > 0
let tt1 = One 1
let tt2 = Two(2, tt1, 3, tt1)
let tt3 = Two(-2, tt1, 4, tt1)
let tt4 = One 0

// Problem 4 (30%)
type Trie<'a> = N of 'a * bool * Children<'a>

and Children<'a> = Trie<'a> list

let t1 = N(0, false, [ N(0, false, [ N(1, true, []) ]) ])
let t2 = N(0, true, [ N(0, false, [ N(1, true, []) ]) ])

let ta = N(1, true, [ N(2, true, []) ])
let tb = N(3, false, [ N(0, true, []) ])
let tc = N(2, true, [])

let t3 = N(0, false, [ ta; tb; tc ])

// Point 1
let rec countNodes (N (_, _, xs): Trie<'a>) = 1 + List.sumBy countNodes xs

// Point 2
type Word<'a> = 'a list

let rec accept (w: Word<'a>) (N (x, y, ls): Trie<'a>) =
    match w with
    | [] -> true
    | [ w1 ] when x = w1 -> true
    | head :: tail when x = head -> List.exists (accept tail) ls
    | _ -> false

// Point 3
let rec wordsOf' (N (a, b, ch): Trie<'a>) =
    match b with
    | true -> [ [ a ] ] @ (ch |> List.collect wordsOf' |> List.map (fun x -> a :: x))
    | false -> ch |> List.collect wordsOf' |> List.map (fun x -> a :: x)

let rec wordsOf t = t |> wordsOf' |> Set.ofList

// Point 4
let rec uselessLeaves (N (a, b, ch): Trie<'a>) =
    match ch with
    | [] when b -> false
    | [] -> true
    | ch -> List.exists uselessLeaves ch

// Point 5
let rec degree (N (a, b, ch): Trie<'a>) =
    List.max (List.length ch :: List.map degree ch)
