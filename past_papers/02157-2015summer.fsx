// Problem 1 (20%)
// Point 1
let rec repeat s n =
    match n with
    | 0 -> ""
    | n when n < 0 -> failwith "ArgException"
    | _ -> s + repeat s (n - 1)

let repeat1 s n = String.replicate n s

// Point 2
let rec f (s1: string) (s2: string) n =
    match n % 2 = 0 with
    | _ when n = 0 -> ""
    | _ when n < 0 -> failwith "ArgException"
    | true -> s1 + "\n" + f s1 s2 (n - 1)
    | _ -> s2 + "\n" + f s1 s2 (n - 1)

// Point 3
let rec viz m n = f (repeat "OX" m) (repeat "XO" m) 5

// Point 4
let rec repeat' (s: string) (acc: string) n =
    match n with
    | 0 -> acc
    | n when n < 0 -> failwith "ArgException"
    | n -> repeat' s (acc + s) (n - 1)

// CONTINUATION BASED TOO HARD!!!!!!!!!

// Problem 2 (20%)
// Point 1
let rec mixMap f ls1 ls2 =
    match ls1, ls2 with
    | [], [] -> []
    | x :: xs, y :: ys -> (f x y) :: mixMap f xs ys
    | _ -> failwith "Can't mixMap unequal lists!"

let rec mixMap' f = List.map2 (f)
// Point 2
let rec unmixMap f g (ls: ('a * 'b) list) =
    match ls with
    | [] -> ([], [])
    | (x, y) :: tail -> let xs, ys = unmixMap f g tail in (f x :: xs, g y :: ys)

let rec unmixMap' f g ls =
    let x, y = List.unzip ls in List.map (f) x, List.map (g) y

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
let rec getSum t =
    match t with
    | Lf -> 0
    | Br (t1, a, t2) -> a + getSum t1 + getSum t2

let rec increment n t =
    match t with
    | Lf -> Lf
    | Br (t1, a, t2) -> Br(increment (n + a) t1, n + a, increment (getSum t1 + a + n) t2)

let accumulate = increment 0

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

let isValidCourseDesc ((_, ects): CourseDesc) = ects <> 0 && (ects % 5 = 0)

// Point 2
let rec isValidCourseBase (cb: CourseBase) =
    Map.forall (fun _ -> isValidCourseDesc) cb

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

// Point 3
let rec disjoint (set1: Set<'a>) (set2: Set<'a>) =
    (set1, set2) ||> Set.difference |> Set.isEmpty

// Point 4
let sumECTS (set: Set<CourseNo>) (cb: CourseBase) =
    set |> Set.toList |> List.sumBy (fun x -> Map.find x cb |> snd)

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
    && Set.forall (fun x -> Map.containsKey x cb) plan
