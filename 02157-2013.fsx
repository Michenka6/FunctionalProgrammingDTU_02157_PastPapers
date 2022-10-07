// Problem 1 (30%)
type Multiset<'a when 'a: equality> = ('a * int) list

let example: Multiset<string> = [ ("b", 3); ("a", 5); ("d", 1) ]

// Point 1
let rec inv (ms: Multiset<'a>) =
    match ms with
    | [] -> true
    | head :: tail -> tail |> List.forall (fun (x, _) -> x <> fst head) && inv tail

// Point 2
let rec insert a n (ms: Multiset<'a>) =
    match ms with
    | [] -> [ (a, n) ]
    | (x, y) :: tail when a = x -> (x, (y + n)) :: tail
    | head :: tail -> head :: insert a n tail

// Point 3
// 'a -> Multiset<'a> -> int when 'a equality
let numberOf' e (ms: Multiset<'a>) =
    ms |> List.filter (fun (x, _) -> x = e) |> List.head |> snd

// Point 4
let delete' e (ms: Multiset<'a>) =
    ms |> List.map (fun (x, n) -> if x = e then (x, n - 1) else (x, n))

// Point 5
let rec union ((ms1: Multiset<'a>), (ms2: Multiset<'a>)) =
    match ms2 with
    | [] -> ms1
    | (x, y) :: tail -> union ((insert x y ms1), tail)

let union' ((ms1: Multiset<'a>), (ms2: Multiset<'a>)) =
    ms2 |> List.fold (fun ms (x, y) -> insert x y ms) ms1

type MultisetMap<'a when 'a: comparison> = Map<'a, int>

let exampleMap: MultisetMap<string> =
    Map.empty |> Map.add "b" 3 |> Map.add "a" 5 |> Map.add "d" 1

// Point 6
let inv' (msm: MultisetMap<string>) = msm |> Map.forall (fun x y -> y > 0)

let insert'' a n (msm: MultisetMap<string>) =
    match (msm |> Map.tryFind a) with
    | Some x -> (msm |> Map.add a (x + n))
    | None -> (msm |> Map.add a n)

let union'' (msm1: MultisetMap<string>) (msm2: MultisetMap<string>) =
    msm2 |> Map.fold (fun msm a b -> msm |> Map.add a b) msm1

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

// Still don't understand continuation based tail-recursion.

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
let rec maxL = List.max

// Point 2
let overview' (book: Book) = book |> List.map fst

// Point 3
let rec depthSection' ((x, xs): Section) = xs |> List.map depthElem' |> List.max

and depthElem' (elem: Elem) =
    match elem with
    | Par _ -> 0
    | Sub x -> depthSection' x

let depthChapter' ((x, xs): Chapter) =
    xs |> List.map (fun x -> 1 + depthSection' x) |> List.max

let depthBook' (book: Book) =
    book |> List.map (fun x -> depthChapter' x) |> List.max

type Numbering = int list
type Entry = Numbering * Title
type Toc = Entry list

// Point 4
let rec toc (book: Book) : Toc =
    let rec sectionAux n m k (l, ls) : Toc =
        match ls with
        | [] -> []
        | Par x :: tail -> sectionAux n m k (l, tail)
        | Sub (x, _) :: tail -> ([ n; m; k ], x) :: sectionAux n m (k + 1) (l, tail)

    let rec chapterAux n m (chapter: Chapter) : Toc =
        match chapter with
        | (_, []) -> []
        | (x, (a, b) :: tail) -> (([ n; m ], a) :: sectionAux n m 1 (a, b)) @ chapterAux n (m + 1) (x, tail)

    let rec bookAux n (book: Book) : Toc =
        match book with
        | [] -> []
        | (x, xs) :: tail -> (([ n ], x) :: chapterAux n 1 (x, xs)) @ bookAux (n + 1) tail

    bookAux 1 book
