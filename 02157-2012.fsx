// Problem 1 (25%)
type Name = string
type Score = int
type Result = Name * Score

// Point 1
let res1: Result = ("Amy", 10)
let res2: Result = ("Doctor", 15)
let res3: Result = ("Rory", 5)
let results: Result list = [ res1; res2; res3 ]

let legalResults (results: Result list) =
    results |> List.forall (fun (_, x) -> x <= 100 && x >= 0)

// Point 2
let maxScore' (resutls: Result list) = results |> List.maxBy snd |> fst

// Point 3
let best (results: Result list) = results |> List.maxBy snd

// Point 4
let average (results: Result list) =
    let len = results |> List.length |> float
    results |> List.fold (fun x (_, y) -> x + (float y / len)) 0.0

// Point 5
let delete (res: Result) (results: Result list) =
    results |> List.filter (fun x -> x <> res)

// Point 6
let bestN (results: Result list) n =
    results |> List.sortBy (fun (_, y) -> y) |> List.rev |> List.take n

// Problem 2 (35%)
type Typ =
    | Integer
    | Boolean
    | Ft of Typ list * Typ

type Decl = string * Typ

// Point 1
let decs: Decl list = [ ("2", Integer); ("True", Boolean); ("1", Integer) ]

let distinctVars (declarations: Decl list) =
    declarations |> Set.ofList |> Set.count |> (=) (declarations |> List.length)

type SymbolTable = Map<string, Typ>

// Point 2
let toSymbolTable (declarations: Decl list) = declarations |> Map.ofList
// Point 3
let extendST (sym: SymbolTable) (declarations: Decl list) =
    let sym1 = toSymbolTable declarations
    Map.fold (fun acc a b -> acc |> Map.add a b) sym sym1

type Exp =
    | V of string
    | A of string * Exp list

let expression = A(">", [ V "x"; V "y" ])

let madd: SymbolTable =
    Map.empty |> Map.add ">" Boolean |> Map.add "x" Integer |> Map.add "y" Integer

// Point 4
let rec symbolsDefined (sym: SymbolTable) (exp: Exp) =
    match exp with
    | V x -> sym |> Map.containsKey x
    | A (x, xs) -> sym |> Map.containsKey x && xs |> List.forall (symbolsDefined sym)

// Point 5
let rec typOf (sym: SymbolTable) (exp: Exp) =
    match exp with
    | V x -> (sym |> Map.tryFind x).Value
    | A (x, xs) -> (sym |> Map.tryFind x).Value

type Stm =
    | Ass of string * Exp
    | Seq of Stm * Stm
    | Ite of Exp * Stm * Stm
    | While of Exp * Stm
    | Block of Decl list * Stm

// Point 6
let rec wellTyped (sym: SymbolTable) (stm: Stm) =
    match stm with
    | Ass (x, e) -> sym |> Map.containsKey x && sym |> Map.find x = typOf sym e
    | Seq (stm1, stm2) -> wellTyped sym stm1 && wellTyped sym stm2
    | Ite (e, stm1, stm2) -> typOf sym e = Boolean && wellTyped sym stm1 && wellTyped sym stm2
    | While (e, stm) -> typOf sym e = Boolean && wellTyped sym stm
    | Block (decls, stm) -> distinctVars decls && wellTyped (extendST sym decls) stm

// Problem 3 (20%)
let rec h a b =
    match a with
    | [] -> b
    | c :: d -> c :: (h d b)

type T<'a, 'b> =
    | A of 'a
    | B of 'b
    | C of T<'a, 'b> * T<'a, 'b>

let rec f1 (t: T<'a, 'b>) =
    match t with
    | C (t1, t2) -> 1 + max (f1 t1) (f1 t2)
    | _ -> 1

let rec f2 =
    function
    | A e
    | B e -> [ e ]
    | C (t1, t2) -> f2 t1 @ f2 t2

let rec f3 e b t =
    match t with
    | C (t1, t2) when b -> C(f3 e b t1, t2)
    | C (t1, t2) -> C(t1, f3 e b t2)
    | _ when b -> C(A e, t)
    | _ -> C(t, B e)

// Point 1
(*
  h : 'a list -> 'a list -> 'a list
  it concatenates the two lists a and b
*)

// Point 2
let a1 = A 2
let b1 = B true
let c1 = C(a1, b1)

// Point 3
let c2 = C(A [ 2; 3; 4 ], B(Some true))

// Point 4
// f1 : T<'a, 'b> -> int
// f1 counts the number of nodes in a tree

// f2 :  T<'a, 'a> -> 'a list
// f2 makes a list of values from the nodes in a tree

// f3 : 'a -> bool -> T <'a, 'a> -> T <'a, 'a>
// f3 if b then it goes to the left branch otherwise right, and if it encounters a leaf it replaces it by a branch (A e, t) or (t, B e) accordingly

// Problem 4 (20%)
type 'a tree =
    | Lf
    | Br of 'a * 'a tree * 'a tree

let rec sumTree =
    function
    | Lf -> 0
    | Br (x, t1, t2) -> x + sumTree t1 + sumTree t2

let rec toList =
    function
    | Lf -> []
    | Br (x, t1, t2) -> x :: (toList t1 @ toList t2)

let rec sumList =
    function
    | [] -> 0
    | x :: xs -> x + sumList xs

let rec sumListA n =
    function
    | [] -> n
    | x :: xs -> sumListA (n + x) xs

// TRIVIAL
