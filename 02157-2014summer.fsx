// Problem 1 (30%)
let rec f n =
    function
    | 0 -> 1
    | k when k > 0 -> n * (f n (k - 1))
    | _ -> failwith "illegal argument"

let rec g p f =
    function
    | [] -> []
    | x :: xs when p x -> f x :: g p f xs
    | _ :: xs -> g p f xs

type T =
    | A of int
    | B of string
    | C of T * T

let rec h =
    function
    | A n -> string n
    | B s -> s
    | C (t1, t2) -> h t1 + h t2

let sq = Seq.initInfinite (fun i -> 3 * i)

let k j =
    seq {
        for i in sq do
            yield (i, i - j)
    }

let xs = Seq.toList (Seq.take 4 sq)

let ys = Seq.toList (Seq.take 4 (k 2))

// Point 1
let p11 = f 4 3

let p12 = g (fun x -> x > 0) f [ 2; 3; -1 ]

let p13 = h (C(A 2, B " what "))

// Point 2
(*
  f : int -> int -> int
  f returns the power of n to the k

  g : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
  g filters a list by p and maps f onto it

  h : T -> string
  h extracts into a string all values from a tree
*)

// Point 3
let rec f' acc n m =
    match m with
    | 0 -> acc
    | k when k > 0 -> f' (acc * n) n (k - 1)
    | _ -> failwith "Illegal argument"

// CAN'T CONTINUATION BASED

// Point 4
(*
  sq : Seq<int> sq is a sequence of multiples of 3 including 0

  k : int -> seq<int>
  k returns tuples of multiples of 3 and them minus j
*)

// Point 5
(*
  xs = [0; 3; 6; 9]
  ys = [(0,-2); (3,1); (6,4); (9,7)]
*)

// Problem 2 (30%)
// Point 1
let rec ordered ls =
    match ls with
    | []
    | [ _ ] -> true
    | x :: (y :: tail) -> (x <= y) && ordered (y :: tail)

// Point 2
let smallerThanAll x xs = xs |> List.forall (fun a -> x < a)

// Point 3
let insertBefore (p: ('a -> bool)) x xs =
    let bools = xs |> List.map p

    let (a, b) =
        (bools |> List.filter (fun x -> x = false), bools |> List.filter (fun x -> x = true))

    a @ [ x ] @ b

// Point 4
type Sex =
    | M
    | F

let rec sexToString (sex: Sex) =
    match sex with
    | M -> "Male"
    | F -> "Female"

// Point 5
let replicate n str =
    [ 1..n ] |> List.fold (fun s _ -> s + str) ""

// Problem 3 (40%)
type Name = string
type YearOfBirth = int
type FamilyTree = P of Name * Sex * YearOfBirth * Children

and Children = FamilyTree list

let peter = P("Peter", M, 2005, [])
let bob = P("Bob", M, 2008, [])
let eve = P("Eve", F, 2010, [])

let fred = P("Fred", M, 1970, [])
let joan = P("Joan", F, 1975, [])
let stanley = P("Stanley", M, 1975, [])
let mary = P("Mary", F, 1980, [ peter; bob; eve ])
let jane = P("Jane", F, 1985, [])

let may = P("May", F, 1945, [ fred; joan ])
let joe = P("Joe", M, 1950, [ stanley; mary; jane ])
let paul = P("Paul", M, 1955, [])

let larry = P("Larry", M, 1920, [ may; joe; paul ])

// Point 1
let rec isWF (t: FamilyTree) =
    match t with
    | P (_, _, _, []) -> true
    | P (a, b, x, xs) ->
        let ages = xs |> List.map (fun (P (_, _, y, _)) -> y)

        x < (ages |> List.max)
        && ordered ages
        && xs |> List.map isWF |> List.fold (&&) true

// Point 2
let makePerson (name, sex, year) = P(name, sex, year, [])

// Point 3
let rec insertChildOf n (P (cn, cs, cy, cc): FamilyTree) (P (name, sex, year, children): FamilyTree) =
    match children with
    | _ when name = n && year < cy -> Some(P(name, sex, year, insertChildOfInList n (P(cn, cs, cy, cc)) children))
    | head :: tail ->
        let present = insertChildOf n (P(cn, cs, cy, cc)) head

        if present = None then
            insertChildOf n (P(cn, cs, cy, cc)) (P(name, sex, year, tail))
        else
            present
    | _ -> None

and insertChildOfInList n (P (cn, cs, cy, cc): FamilyTree) (children: FamilyTree list) =
    match children with
    | [] -> [ (P(cn, cs, cy, cc): FamilyTree) ]
    | P (x, y, z, c) :: tail when z <= cy -> P(x, y, z, c) :: ((P(cn, cs, cy, cc)) :: tail)
    | head :: tail -> head :: insertChildOfInList n (P(cn, cs, cy, cc)) tail

// Point 4
let rec find n (P (name, sex, year, children): FamilyTree) =
    match children with
    | _ when name = n -> [ (sex, year, children |> List.map (fun (P (x, _, _, _)) -> x)) ]
    | [] -> []
    | children -> children |> List.map (find n) |> List.concat

// Point 5
let rec toString n (P (name, sex, year, children): FamilyTree) =
    replicate (n - 6) " "
    + name
    + " "
    + sexToString sex
    + " "
    + string year
    + "\n"
    + (children |> List.map (toString (n + 6)) |> List.fold (+) "")

// Point 6
let rec truncate (P (name, sex, year, children): FamilyTree) =
    match children with
    | _ when sex = F -> P(name, sex, year, [])
    | children -> P(name, sex, year, children |> List.map truncate)
