// Problem 1 (25%)
type Name = string;;
type Score = int;;
type Result = Name * Score;;

// Point 1
let res1:Result = ("Amy", 10);;
let res2:Result = ("Doctor", 15);;
let res3:Result = ("Rory", 5);;
let results:Result list = [res1; res2; res3];;

let rec legalResults (results:Result list) =
  match results with
  | [] -> true
  | (a,b) :: tail -> (b <= 100 && b >= 0) && legalResults tail;;

// Point 2
let max a b = if a < b
                        then b
                        else a;;

let rec maxScore (results:Result list) =
  match results with
  | [] -> 0
  | [(a,b)] -> b
  | (a,b) :: tail -> max b (maxScore tail);;

// Point 3
let rec best (results:Result list) :Result =
  match results with
  | [] -> ("EMPTY", 0)
  | [(x, y)] -> (x, y)
  | (x, y) :: ((x1, y1) :: tail) when y < y1 -> best ((x1, y1) :: tail)
  | (x, y) :: ((x1, y1) :: tail) -> best ((x, y) :: tail);;

// Point 4
let average (results:Result list) =
  let rec aux sum n (results:Result list) =
    match results with
    | [] -> sum / n
    | (_, x) :: tail -> aux (sum + float x) (n + 1.0) tail

  aux 0.0 0.0 results;;

// Point 5
let rec delete ((name,score):Result) (results:Result list) =
  match results with
  | [] -> []
  | (x,y) :: tail when x = name && y = score -> tail
  | (x,y) :: tail -> (x,y) :: delete (name,score) tail;;

// Point 6
let rec bestN (results:Result list) n =
    if List.length results < n 
      then failwith "Asking for too many elements"
      else if n <= 0
            then []
            else best results :: bestN (delete (best results) results) (n - 1);;
  
// Problem 2 (35%)
type Typ =
  | Integer
  | Boolean
  | Ft of Typ list * Typ

type Decl = string * Typ

// Point 1
let decs:Decl list = [("2", Integer); ("True", Boolean); ("1", Integer)];;

let rec distinctVars (declarations:Decl list) = 
  match declarations with
  | [] -> true
  | head :: tail -> not (tail |> List.contains head) && distinctVars tail;;

type SymbolTable = Map<string, Typ>
  // Point 2
let toSymbolTable (declarations:Decl list) = 
  declarations
    |> List.fold (fun (map:SymbolTable) (a, b) -> map |> Map.add a b) Map.empty;;

// Point 3
let extendST (sym:SymbolTable) (declarations:Decl list) =
  declarations
    |> List.fold (fun (map:SymbolTable) (a,b) -> map |> Map.add a b) sym;;

type Exp =
  | V of string
  | A of string * Exp list;;

let expression = A (">", [V "x"; V "y"]);;
let madd:SymbolTable =
  Map.empty
  |> Map.add ">" Boolean
  |> Map.add "x" Integer
  |> Map.add "y" Integer;;

// Point 4
let rec symbolsDefined (sym:SymbolTable) (exp:Exp) =
  match exp with
  | V x -> not ((sym |> Map.tryFind x) = None)
  | A (x, xs) -> not ((sym |> Map.tryFind x) = None) && aux sym xs

and aux (sym:SymbolTable) (expLs:Exp list) =
  match expLs with
  | [] -> true
  | head :: tail -> symbolsDefined sym head && aux sym tail;;

// Point 5
let rec typOf (sym:SymbolTable) (exp:Exp) =
  match exp with
  | V x -> aux' (sym |> Map.tryFind x)
  | A (x, xs) -> aux' (sym |> Map.tryFind x)

and aux' (typ:Typ option) =
  match typ with
  | Some x -> x
  | None -> failwith "Not well typed";;

type Stm =
  | Ass of string * Exp
  | Seq of Stm * Stm
  | Ite of Exp * Stm * Stm
  | While of Exp * Stm
  | Block of Decl list * Stm;;

// Point 6
let rec wellTyped (sym:SymbolTable) (stm:Stm) =
  match stm with
  | Ass (x, e) -> ((sym |> Map.tryFind x) = None) && ((sym |> Map.find x) = (typOf sym e))
  | Seq (stm1, stm2) -> wellTyped sym stm1 && wellTyped sym stm2
  | Ite (e, stm1, stm2) -> (typOf sym e = Boolean) && wellTyped sym stm1 && wellTyped sym stm2
  | While (e, stm) -> (typOf sym e = Boolean) && wellTyped sym stm
  | Block (decls, stm) -> distinctVars decls && wellTyped (extendST sym decls) stm;;

// Problem 3 (20%)
let rec h a b =
  match a with
  | [] -> b
  | c :: d -> c :: (h d b);;

type T<'a, 'b> =
  | A of 'a
  | B of 'b
  | C of T<'a, 'b> * T<'a, 'b>;;

let rec f1 (t:T<'a,'b>) =
  match t with
  | C (t1,t2) -> 1 + max (f1 t1) (f1 t2)
  | _ -> 1;;

let rec f2 = function
  |A e |B e ->[e]
  |C(t1,t2)->f2 t1 @ f2 t2;;

let rec f3 e b t=
  match t with
  | C (t1,t2) when b -> C(f3 e b t1, t2)
  | C (t1,t2) -> C (t1,f3 e b t2)
  | _ when b -> C (A e,t)
  | _ -> C (t, B e);;

// Point 1
(*
  h : 'a list -> 'a list -> 'a list
  it concatenates the two lists a and b
*)

// Point 2
let a1 = A 2;;
let b1 = B true;;
let c1 = C (a1, b1);;

// Point 3
let c2 = C (A [2; 3; 4], B (Some true));;

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
  | Br of 'a * 'a tree * 'a tree;;

let rec sumTree = function
  | Lf -> 0
  | Br (x, t1, t2) -> x + sumTree t1 + sumTree t2;;

let rec toList = function
  | Lf -> []
  | Br (x, t1, t2) -> x :: (ToList t1 @ toList t2);;

let rec sumList = function
  | [] -> 0
  | x :: xs -> x + sumList xs;;

let rec sumListA n = function
  | [] -> n
  | x :: xs -> sumListA (n + x) xs;;

// TRIVIAL