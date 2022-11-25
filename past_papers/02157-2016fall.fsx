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

let insert' ((n, e, p): Score) (sb: Scoreboard) =
    let (x, y) = List.partition (fun (_, _, x) -> x <= p) sb
    x @ [ (n, e, p) ] @ y

// Point 3
let rec get ((name: Name), (sb: Scoreboard)) =
    match sb with
    | [] -> failwith "Could not find!"
    | (n, e, p) :: tail when n = name -> (e, p)
    | _ :: tail -> get (name, tail)

let get' ((name: Name), (sb: Scoreboard)) =
    sb
    |> List.filter (fun (x, _, _) -> x = name)
    |> List.map (fun (_, e, p) -> (e, p))

// Point 4
let rec top k (sb: Scoreboard) =
    if k < 0 || k > (List.length sb) then
        None
    else
        Some(sb |> List.take k)

// Problem 2 (15%)
// Point 1
let rec replace a b xs =
    match xs with
    | [] -> []
    | head :: tail when head = a -> b :: replace a b tail
    | head :: tail -> head :: replace a b tail

let replace' a b =
    List.map (fun x -> if x = a then b else a)

// Point 2
(*
  replace : 'a -> 'a -> 'a list -> 'a list
*)

// Point 3
let rec replace'' a b acc xs =
    match xs with
    | [] -> acc
    | head :: tail when head = a -> replace'' a b (acc @ [ b ]) tail
    | head :: tail -> replace'' a b (acc @ [ a ]) tail

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
let rec f' k =
    function
    | A a -> k ([ a ], [])
    | B b -> k ([], [ b ])
    | Node (t1, t2) -> f' (fun (xs1, ys1) -> f' (fun (xs2, ys2) -> (xs1 @ xs2, ys1 @ ys2)) t2) t1

// Problem 5 (30%)
type T<'a> = N of 'a * T<'a> list

let td = N("g", [])
let tc = N("c", [ N("d", []); N("e", [ td ]) ])
let tb = N("b", [ N("c", []) ])
let ta = N("a", [ tb; tc; N("f", []) ])

// Point 1
let rec toList (N (x, xs): T<'a>) = x :: List.collect toList xs

// Point 2
let rec map f (N (x, xs): T<'a>) = N(f x, List.map (map f) xs)

type Path = int list

// Point 3
let rec isPath (path: Path) (N (x, xs): T<'a>) =
    match path with
    | [] -> true
    | head :: tail when head <= (List.length xs) - 1 -> (head, xs) ||> List.item |> isPath tail
    | _ -> false

// Point 4
let rec get1 (path: Path) (N (x, xs): T<'a>) =
    match path with
    | [] -> N(x, xs)
    | head :: tail -> (head, xs) ||> List.item |> get1 tail

// Point 5
let rec allPaths (path: Path) (N (x, xs): T<'a>) =
    [ List.rev path ] :: List.mapi (fun i x -> allPaths (i :: path) x) xs
    |> List.concat

let getKey (N (x, _): T<'a>) = x

let tryFindPathto (v: 'a) (t: T<'a>) =
    let validPaths = allPaths [] t |> List.filter (fun x -> getKey (get1 x t) = v)

    match validPaths with
    | [] -> None
    | x :: xs -> Some(x)
