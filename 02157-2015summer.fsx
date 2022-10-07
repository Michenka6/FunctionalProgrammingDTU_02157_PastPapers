// Problem 1 (20%)
// Point 1
let rec repeat s n =
    [ 0 .. n - 1 ] |> List.fold (fun acc _ -> acc + s) ""

// Point 2
let rec f (s1: string) (s2: string) n =
    [ 0 .. n - 1 ]
    |> List.map (fun x -> if x % 2 = 0 then s1 + "\n" else s2 + "\n")
    |> List.fold (+) ""

// Point 3
let rec viz m n =
    [ 0 .. n - 1 ]
    |> List.map (fun x ->
        if x % 2 = 0 then
            repeat "XO" m + "\n"
        else
            repeat "OX" m + "\n")
    |> List.fold (+) ""


// Point 4
let rec repeat' (s: string) (acc: string) n =
    match n with
    | 0 -> acc
    | n when n < 0 -> failwith "Can't repeat a negative number of times!"
    | n -> repeat' s (acc + s) (n - 1)

// CONTINUATION BASED TOO HARD!!!!!!!!!

// Problem 2 (20%)
// Point 1
let rec mixMap f ls1 ls2 =
    match (ls1, ls2) with
    | ([], []) -> []
    | (x :: xx, y :: yy) -> (f x, f y) :: mixMap f xx yy
    | _ -> failwith "Can't mixMap unequal lists!"

// Point 2
let rec unmixMap f g (ls: ('a * 'b) list) =
    let rec aux f g (a, b) (ls: ('a * 'b) list) =
        match ls with
        | [] -> (List.rev a, List.rev b)
        | (a1, b1) :: xs -> aux f g (f a1 :: a, g b1 :: b) xs

    aux f g ([], []) ls

// Point 3
(*
  mixMap : ('a -> 'b) -> 'a list -> 'a list -> ('b * 'b) list

  ummixMap : ('a -> 'b) -> ('c -> 'd) -> ('a * 'c) list -> ('b list * 'd list) 
*)

// Problem 3 (30%)
type Tree<'a> =
    | Lf
    | Br of Tree<'a> * 'a * Tree<'a>

let t: Tree<int> =
    Br(Br(Br(Lf, 1, Lf), 2, Br(Lf, 3, Lf)), 4, Br(Br(Lf, 5, Lf), 6, Br(Lf, 7, Lf)))

// Point 1
let rec reflect (t: Tree<'a>) =
    match t with
    | Lf -> Lf
    | Br (t1, a, t2) -> Br((reflect t2), a, (reflect t1))

// Point 2
// ACCUMULATE?!?

let rec k i t =
    match t with
    | Lf -> Lf
    | Br (tl, a, tr) -> Br(k (i * i) tl, i * a, k (i * i) tr)

let rec h n m t =
    match t with
    | Br (tl, a, tr) when n = m -> h n 1 tl @ [ a ] @ h n 1 tr
    | Br (tl, _, tr) -> h n (m + 1) tl @ h n (m + 1) tr
    | Lf -> []

// Point 3
(*
  k : int -> Tree<int> -> Tree<int>
  k multiplies each key in a tree quadrativcally as it goes down the layers

  h : int -> int -> Tree<'a> -> 'a list
  h returns a list of every nth layer
*)

// Problem 4 (30%)
type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS

type CourseBase = Map<CourseNo, CourseDesc>

// Point 1
let func: CourseDesc = ("Functional Programming", 5)
let java: CourseDesc = ("Object-Oriented Programming", 10)
let math: CourseDesc = ("Advanced Engineering Mathematics I", 10)

let courses: CourseBase =
    Map.empty |> Map.add 02157 func |> Map.add 02160 java |> Map.add 01006 math

let isValidCourseDesc ((_, ects): CourseDesc) = not (ects = 0) && (ects % 5 = 0)

// Point 2
let rec isValidCourseBase (cb: CourseBase) =
    cb |> Map.values |> Seq.forall isValidCourseDesc

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

// Point 3
let rec disjoint (set1: Set<'a>) (set2: Set<'a>) =
    set1 |> Set.difference set2 |> Set.isEmpty

// Point 4
let flip f a b = f b a

let sumECTS (set: Set<CourseNo>) (cb: CourseBase) =
    set |> Set.map (flip Map.find cb) |> Set.map snd |> Set.fold (+) 0

// Point 5
let rec isValidCourseGroup ((man, opt): CourseGroup) (cb: CourseBase) =
    (disjoint man opt)
    && (sumECTS man cb <= 45)
    && ((sumECTS man cb = 45 && sumECTS opt cb = 0) || not (sumECTS opt cb = 0))
    && sumECTS man cb + sumECTS opt cb >= 45

type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>

// Point 6
let isValid ((bns, tc, ppc, ep): FlagModel) (cb: CourseBase) =
    isValidCourseGroup bns cb
    && isValidCourseGroup tc cb
    && isValidCourseGroup ppc cb

// Point 7
let checkPlan (plan: CoursePlan) ((bns, tc, ppc, ep): FlagModel) (cb: CourseBase) =
    isValid (bns, tc, ppc, ep) cb
    && isValidCourseBase cb
    && plan |> Set.forall (flip Map.containsKey cb)
