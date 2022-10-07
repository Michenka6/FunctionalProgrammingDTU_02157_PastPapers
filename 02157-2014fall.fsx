// Problem 1 (20%)
type Rel<'a, 'b> = ('a * 'b list) list

let rel: Rel<int, string> = [ (1, [ "a"; "b"; "c" ]); (4, [ "b"; "e" ]) ]

// Point 1
let apply a (rel: Rel<'a, 'b>) =
    match rel |> List.filter (fun (x, _) -> x = a) with
    | [] -> []
    | (a, b) :: xs -> b

// Point 2
let inRelation x y (rel: Rel<'a, 'b>) = rel |> apply x |> List.contains y

// Point 3
let insert x y (rel: Rel<'a, 'b>) =
    if rel |> List.map fst |> List.contains x then
        rel |> List.map (fun (a, b) -> if x = a then (a, y :: b) else (a, b))
    else
        ((x, [ y ]) :: rel)

// Point 4
let toRel (ls: ('a * 'b) list) : Rel<'a, 'b> =
    List.fold (fun acc (x, y) -> insert x y acc) [] ls

// Problem 2 (25%)
// Point 1
let multTable n =
    (+) 1 |> Seq.initInfinite |> Seq.filter (fun x -> x % n = 0) |> Seq.take 10

// Point 2
let rec tableOf x y f =
    seq {
        for i in [ 1..x ] do
            for j in [ 1..y ] do
                yield (i, j, f i j)
    }

// Point 3
let rec repeatChar x =
    match x with
    | 0 -> ""
    | n -> "a" + repeatChar (n - 1)

let aSeq = (+) 1 |> Seq.initInfinite |> Seq.map (fun x -> repeatChar x)

let rec f i =
    function
    | [] -> []
    | x :: xs -> (x + i) :: f (i * i) xs

// Point 4
(*
  f takes in a list of integers and returns a list of increments of each list by a square of its negative index
*)

// Point 5
let rec f' acc i =
    function
    | [] -> List.rev acc
    | x :: xs -> f' ((x + i) :: acc) (i * 1) xs

// Continuation based tail-recursion is not for me :(

// Problem 3 (20%)
type T<'a> = N of 'a * T<'a> list

let rec fa (N (e, es)) = e :: g es

and g =
    function
    | [] -> []
    | e :: es -> fa e @ g es

let rec h p =
    function
    | N (e, _) when p e -> N(e, [])
    | N (e, es) -> N(e, List.map (h p) es)

let rec k (N (_, es)) = 1 + List.fold max 0 (List.map k es)

// Point 1
let t1 = N(1, [])
let t2 = N(2, [])
let t3 = N(3, [ t1; t2 ])

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
type Outcome =
    | S
    | F

type Sample = Outcome list

type ProbTree =
    | Branch of string * float * ProbTree * ProbTree
    | Leaf of string

let exp =
    Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D"))

// Point 1
let rec probOK (t: ProbTree) =
    match t with
    | Leaf _ -> true
    | Branch (_, x, b1, b2) -> (0.0 <= x && x <= 1.0) && probOK b1 && probOK b2

// Point 2
let rec isSample ((os: Sample), (t: ProbTree)) =
    match (os, t) with
    | ([], Leaf _) -> true
    | (x :: xs, Branch (_, _, tl, tr)) ->
        match x with
        | S -> isSample (xs, tr)
        | F -> isSample (xs, tl)
    | _ -> false

// Point 3
type Description = (Outcome * string) list * float * string

let ds: Description = ([ (F, ">2"); (S, ">3") ], 0.165, "C")

let rec descriptionOf (os: Sample) (t: ProbTree) =
    let rec aux (os: Sample) (t: ProbTree) ((ls, n, str): Description) =
        match (os, t) with
        | ([], Leaf x) -> (ls |> List.rev, n, x)
        | (x :: xs, Branch (s, p, tl, tr)) ->
            match x with
            | S -> aux xs tl (((S, s) :: ls), p * n, str)
            | F -> aux xs tr (((F, s) :: ls), (1.0 - p) * n, str)
        | _ -> failwith "Invalid Sample"

    aux os t ([], 1.0, "")

// Point 4
let allDescriptions (t: ProbTree) : Set<Description> =
    let rec aux ((ls, n, str): Description) (t: ProbTree) =
        match t with
        | Leaf x -> [ (List.rev ls, n, x) ]
        | Branch (ds, p, tr, tl) ->
            aux (((S, ds) :: ls), p * n, str) tr
            @ aux (((F, ds) :: ls), (1.0 - p) * n, str) tl

    aux ([], 1.0, "") t |> Set.ofList

// Point 5
let probabilityOf (t: ProbTree) pred =
    allDescriptions t
    |> Set.map (fun (_, p, c) -> if pred c then p else 0.0)
    |> Set.fold (+) 0.0

// Point 6
// TRIVIAL
