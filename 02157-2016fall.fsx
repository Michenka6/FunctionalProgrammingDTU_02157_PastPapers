// Problem 1 (20%)
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point

type Scoreboard = Score list

let sb: Scoreboard =
    [ ("Joe", "June Fishing", 35)
      ("Peter", "May Fishing", 30)
      ("Joe", "May Fishing", 28)
      ("Paul", "June Fishing", 28) ]
// Point 1
let rec inv (sb: Scoreboard) =
    match sb with
    | []
    | [ _ ] -> true
    | (n, e, p) :: ((n1, e1, p1) :: tail) when p >= p1 -> inv ((n1, e1, p1) :: tail)
    | _ -> false

// Point 2
let rec insert ((n, e, p): Score) (sb: Scoreboard) =
    match sb with
    | [] -> [ (n, e, p) ]
    | (n1, e1, p1) :: tail when p >= p1 -> (n, e, p) :: ((n1, e1, p1) :: tail)
    | (n1, e1, p1) :: tail -> (n1, e1, p1) :: insert (n, e, p) tail

// Point 3
let rec get ((name: Name), (sb: Scoreboard)) =
    match sb with
    | [] -> failwith "Could not find!"
    | (n, e, p) :: tail when n = name -> (e, p)
    | _ :: tail -> get (name, tail)

// Point 4
let rec top k (sb: Scoreboard) =
    if k < 0 || k > (List.length sb) then
        None
    else
        Some(fst (sb |> List.splitAt k))

// Problem 2 (15%)
// Point 1
let rec replace a b xs =
    xs |> List.map (fun x -> if x = a then b else a)

// Point 2
(*
  replace : 'a -> 'a -> 'a list -> 'a list
*)

// Point 3
let rec replace' a b acc xs =
    match xs with
    | [] -> acc
    | head :: tail when head = a -> replace' a b (acc @ [ b ]) tail
    | head :: tail -> replace' a b (acc @ [ a ]) tail

// Problem 3 (10%)
let pos = Seq.initInfinite (fun i -> i + 1)

let seq1 =
    seq {
        yield (0, 0)

        for i in pos do
            yield (i, i)
            yield (-i, -i)
    }

let val1 = Seq.take 5 seq1

let nat = Seq.initInfinite id

let seq2 =
    seq {
        for i in nat do
            yield (i, 0)

            for j in [ 1..i ] do
                yield (i, j)
    }

let val2 = Seq.toList (Seq.take 10 seq2)

// Point 1
(*
  pos : seq<int>
  seq1 : seq<int * int>
  val1 : seq<int * int>
*)

// Point 2
(*
  seq2 : seq<int * int>
  val2 : (int * int) list
  val2 = [(0, 0); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2); (3, 0); (3, 1); (3, 2); (3, 3)]
*)

// Problem 4 (25%)
type Tree<'a, 'b> =
    | A of 'a
    | B of 'b
    | Node of Tree<'a, 'b> * Tree<'a, 'b>

// Point 1
let a = A true
let b = B [ 1; 2; 3; 4; 5 ]
let node = Node(a, b)

// Point 2
let rec countA (t: Tree<'a, 'b>) =
    match t with
    | A _ -> 1
    | B _ -> 0
    | Node (x, y) -> countA x + countA y

// Point 3
let rec subst a a' b b' (t: Tree<'a, 'b>) =
    match t with
    | A x when x = a -> A a'
    | B x when x = b -> B b'
    | Node (x, y) -> Node(subst a a' b b' x, subst a a' b b' y)
    | x -> x

let rec g =
    function
    | Node (t1, t2) -> Node(g t2, g t1)
    | leaf -> leaf

let rec f =
    function
    | A a -> ([ a ], [])
    | B b -> ([], [ b ])
    | Node (t1, t2) ->
        let (xs1, ys1) = f t1
        let (xs2, ys2) = f t2
        (xs1 @ xs2, ys1 @ ys2)

// Point 4
(*
  f : Tree<'a, 'b> -> 'a list * 'b list
  f returns a tuple of a leaf values and b leaf values

  g : Tree<'a, 'b> -> Tree<'a, 'b>
  g mirrors a tree
*)

// Point 5
// I CAN'T

// Problem 5 (30%)
type T<'a> = N of 'a * T<'a> list

let td = N("g", [])
let tc = N("c", [ N("d", []); N("e", [ td ]) ])
let tb = N("b", [ N("c", []) ])
let ta = N("a", [ tb; tc; N("f", []) ])

// Point 1
let rec toList (N (x, xs): T<'a>) =
    [ x ] @ (xs |> List.map toList |> List.concat)

// Point 2
let rec map f (N (x, xs): T<'a>) = N(f x, xs |> List.map (map f))

type Path = int list

// Point 3
let rec isPath (path: Path) (N (x, xs): T<'a>) =
    match path with
    | [] -> true
    | head :: tail when head <= (xs |> List.length) - 1 -> isPath tail (xs |> List.item head)
    | _ -> false

// Point 4
let rec get' (path: Path) (N (x, xs): T<'a>) =
    match path with
    | _ when not (isPath path (N(x, xs))) -> []
    | [] -> [ x ]
    | head :: tail -> get' tail (xs |> List.item head)

// Point 5
