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

let sq = Seq.initInfinite (fun i -> i * 3)

let k j =
    seq {
        for i in sq do
            yield (i, i - j)
    }

let xs = (4, sq) ||> Seq.take |> Seq.toList

let ys = (4, k 2) ||> Seq.take |> Seq.toList

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
let rec f' acc n =
    function
    | 0 -> acc
    | k when k > 0 -> f' (acc * n) n (k - 1)
    | _ -> failwith "Illegal argument"

let rec f'' g n =
    function
    | 0 -> g 1
    | k when k > 0 -> f'' (fun x -> g (n * x)) n (k - 1)
    | _ -> failwith "Illegal argument"

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
let ordered ls = ls = List.sort ls

// Point 2
let smallerThanAll x = List.forall (fun a -> x < a)

// Point 3
let insertBefore p x xs =
    List.takeWhile (fun a -> not (p a)) xs
    @ [ x ] @ List.skipWhile (fun a -> not (p a)) xs

// Point 4
type Sex =
    | M
    | F

let rec sexToString (sex: Sex) =
    match sex with
    | M -> "Male"
    | F -> "Female"

// Point 5
let replicate = String.replicate

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
let getAge (P (_, _, x, _)) = x

let rec isWF (P (name, sex, year, children): FamilyTree) =
    match xs with
    | [] -> true
    | _ ->
        year < (children |> List.map getAge |> List.min)
        && children = List.sortBy getAge children

// Point 2
let makePerson (name, sex, year) = P(name, sex, year, [])

// Point 3
let rec insertChildOf n (t: FamilyTree) (P (name, sex, year, children): FamilyTree) =
    match children with
    | _ when name = n && year < getAge t -> Some(P(name, sex, year, insertChildOfInList n t children))
    | [] -> None
    | _ -> children |> List.map (insertChildOf n t) |> List.fold Option.orElse None

and insertChildOfInList n (t: FamilyTree) (children: FamilyTree list) =
    let a, b = List.partition (getAge >> (<=) (getAge t)) children in a @ [ t ] @ b

// Point 4
let getName (P (x, _, _, _)) = x

let rec find n (P (name, sex, year, children): FamilyTree) =
    match children with
    | _ when name = n -> Some(sex, year, List.map getName children)
    | [] -> None
    | _ -> (None, children) ||> List.fold (fun acc c -> Option.orElse acc (find n c))

// Point 5
let rec toString' n m (P (name, sex, year, children): FamilyTree) =
    replicate (n * m) " "
    + name
    + " "
    + sexToString sex
    + " "
    + string year
    + "\n"
    + (("", children) ||> List.fold (fun acc t -> acc + toString' n (m + 1) t))

let toString n = toString' n 0

// Point 6
let rec truncate (P (name, sex, year, children): FamilyTree) =
    if sex = F then
        P(name, sex, year, [])
    else
        P(name, sex, year, List.map truncate children)
