// Problem 1 (30%)
type Multiset<'a when 'a: equality> = ('a * int) list

let example: Multiset<string> = [ ("b", 3); ("a", 5); ("d", 1) ]

// Point 1
let rec inv (ms: Multiset<'a>) : bool = ms = List.distinctBy fst ms

// Point 2
let rec insert a n (ms: Multiset<'a>) : Multiset<'a> =
    match ms with
    | [] -> [ (a, n) ]
    | (x, y) :: tail when a = x -> (x, (y + n)) :: tail
    | head :: tail -> head :: insert a n tail

// Point 3
let numberOf e (ms: Multiset<'a>) : int =
    List.sumBy (fun (x, y) -> if x = e then y else 0) ms

// Point 4
let delete e (ms: Multiset<'a>) : Multiset<'a> =
    List.map (fun (x, n) -> if x = e then (x, n - 1) else (x, n)) ms

// Point 5
let rec union ((ms1: Multiset<'a>), (ms2: Multiset<'a>)) : Multiset<'a> =
    match ms2 with
    | [] -> ms1
    | (x, y) :: tail -> union (insert x y ms1, tail)

let union' ((ms1: Multiset<'a>), (ms2: Multiset<'a>)) =
    (ms1, ms2) ||> List.fold (fun ms (x, y) -> insert x y ms)

type MultisetMap<'a when 'a: comparison> = Map<'a, int>

let exampleMap: MultisetMap<string> = Map.ofList [ ("b", 3); ("a", 5); ("d", 1) ]

// Point 6
let inv1 (msm: MultisetMap<'a>) : bool = Map.forall (fun _ y -> y > 0) msm

let insert1 a n (msm: MultisetMap<'a>) : MultisetMap<'a> =
    match Map.tryFind a msm with
    | Some x -> Map.add a (x + n) msm
    | None -> Map.add a n msm

let union1 (msm1: MultisetMap<'a>) (msm2: MultisetMap<'a>) : MultisetMap<'a> =
    Map.fold (fun msm a b -> Map.add a b msm) msm1 msm2

// Problem 2 (30%)
let rec f i =
    function
    | [] -> []
    | x :: xs -> (i, x) :: f (i * i) xs

type 'a Tree =
    | Lf
    | Br of 'a Tree * 'a * 'a Tree

let rec g p =
    function
    | Lf -> None
    | Br (_, a, t) when p a -> Some t
    | Br (t1, a, t2) ->
        match g p t1 with
        | None -> g p t2
        | res -> res

// Point 1
(*
    f : int -> 'a list -> (int * 'a) list
    f returns a list of tuples just like iteri where indexes are squared

    g : ('a -> bool) -> 'a Tree -> ('a Tree) option
    g returns the first subtree whos head staisfies p
*)

// Point 2
let rec f' acc i =
    function
    | [] -> List.rev acc
    | x :: xs -> f' ((i, x) :: acc) (i * i) xs

let rec f'' k i =
    function
    | [] -> k []
    | x :: xs -> f'' (fun ls -> k ((i, x) :: ls)) (i * i) xs

(*
    Accumulation based tail recursion is much easier to understand and write, continuation based is really nice from the insight you get from it.
*)

let rec h f (n, e) =
    match n with
    | 0 -> e
    | _ -> h f (n - 1, f n e)

let A = Seq.initInfinite id

let B =
    seq {
        for i in A do
            for j in seq { 0..i } do
                yield (i, j)
    }

let C =
    seq {
        for i in A do
            for j in seq { 0..i } do
                yield (i - j, j)
    }

let X = Seq.toList (Seq.take 4 A)
let Y = Seq.toList (Seq.take 6 B)
let Z = Seq.toList (Seq.take 10 C)

// Point 3
// h (*) (4, 1) = 24
// h : ('a -> 'a) -> int * 'a -> 'a
// h applies a mapping function on a set of descending values with a curry.

// Point 4
(*
    A : seq<int>
    B : seq<int * int>
    C : seq<int * int>
    X : int list
    Y : (int * int) list
    Z : (int * int) list
*)

// Problem 3 (40%)
type Title = string

type Section = Title * Elem list

and Elem =
    | Par of string
    | Sub of Section

type Chapter = Title * Section list
type Book = Chapter list

let sec11 = ("Background", [ Par "bla"; Sub(("Why programming", [ Par "Bla." ])) ])
let sec12 = ("An example", [ Par "bla"; Sub(("Special features", [ Par "Bla." ])) ])

let sec21 =
    ("Fundamental concepts", [ Par "bla"; Sub(("Mathematical background", [ Par "Bla." ])) ])

let sec22 =
    ("Operational semantics", [ Sub(("Basics", [ Par "Bla." ])); Sub(("Applications", [ Par "Bla." ])) ])

let sec23 = ("Further reading", [ Par "bla" ])
let sec31 = ("Overview", [ Par "bla" ])
let sec32 = ("A simple example", [ Par "bla" ])
let sec33 = ("An advanced example", [ Par "bla" ])
let sec41 = ("Status", [ Par "bla" ])
let sec42 = ("What's next?", [ Par "bla" ])
let ch1 = ("Introduction", [ sec11; sec12 ])
let ch2 = ("Basic Issues", [ sec21; sec22; sec23 ])
let ch3 = ("Advanced Issues", [ sec31; sec32; sec33; sec23 ])
let ch4 = ("Conclusion", [ sec41; sec42 ])
let book1 = [ ch1; ch2; ch3; ch4 ]

//  Point 1
let rec maxL (ls: 'a list) : 'a = List.max ls

// Point 2
let overview (book: Book) : Title list = List.map fst book

// Point 3
let rec depthSection ((x, xs): Section) : int = xs |> List.map depthElem |> List.max

and depthElem (elem: Elem) : int =
    match elem with
    | Par _ -> 0
    | Sub x -> depthSection x

let depthChapter ((x, xs): Chapter) : int =
    xs |> List.map (fun x -> 1 + depthSection x) |> List.max

let depthBook (book: Book) : int =
    book |> List.map depthChapter |> List.max

type Numbering = int list
type Entry = Numbering * Title
type Toc = Entry list

// Point 4
let rec sectionToc n m (l, ls) : Toc =
    ls
    |> List.filter (fun x ->
        match x with
        | Par _ -> false
        | Sub _ -> true)
    |> List.mapi (fun k el ->
        match el with
        | Par _ -> failwith "unreachable"
        | Sub (x, _) -> ([ n; m; (k + 1) ], x))

let rec chapterToc n ((x, ls): Chapter) : Toc =
    ls
    |> List.mapi (fun m (a, b) -> ([ n; m + 1 ], a) :: sectionToc n (m + 1) (a, b))
    |> List.concat

let rec toc (book: Book) : Toc =
    book
    |> List.mapi (fun n (x, xs) -> ([ n + 1 ], x) :: chapterToc (n + 1) (x, xs))
    |> List.concat
