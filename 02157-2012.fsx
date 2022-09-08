// Problem 1 (25%)
type Name = string;;
type Score = int;;
type Result = Name * Score;;

// Point 1
let results = [("Amy", 10); ("Doctor", 15); ("Rory", 1)];;

let rec legalResults = function
  | [] -> true
  | (a,b) :: tail -> (b < 100 && b >= 0) && legalResults tail;;

// Point 2
let max a b = if a < b
                then b
                else a;;

let rec maxScore = function
  | [] -> 0
  | [(a,b)] -> b
  | (a,b) :: tail -> max b (maxScore tail);;

// Point 3
let best ls =
  let maximum = maxScore ls

  let rec aux = function
    | [] -> ("EMPTY", 0)
    | (a,b) :: tail when b = maximum -> (a,b)
    | _ :: tail -> aux tail
  
  aux ls;;

// Point 4
let average ls =
  let rec aux sum n = function
    | [] -> sum / n
    | (_, x) :: tail -> aux (sum + float x) (n + 1.0) tail

  aux 0.0 0.0 ls;;

// Point 5
let rec delete (a,b) = function
  | [] -> []
  | (x,y) :: tail when x = a && y = b -> tail
  | (x,y) :: tail -> (x,y) :: delete (a,b) tail;;

// Point 6
let rec bestN ls n =
    if List.length ls < n 
      then failwith "Asking for too many elements"
      else if n <= 0
        then []
        else let maxTemp = best ls
             maxTemp :: bestN (delete maxTemp ls) (n - 1);;
  
// Problem 2 (35%)
type Typ =
  | Integer
  | Boolean
  | Ft of Typ list * Typ

type Decl = string * Typ

// Point 1
let decs = [("2", Integer); ("True", Boolean); ("1", Integer)];;

let rec has_one id = function
  | [] -> true
  | (a,b) :: tail when id = a -> false
  | _ :: tail -> has_one id tail;;

let rec distinctVars = function
  | [] -> true
  | [(_,_)] -> true
  | (a,_) :: tail -> (has_one a tail) && distinctVars tail;;


type SymbolTable = Map<string,Typ>
  // Point 2
let toSymbolTable = List.fold (fun (map:SymbolTable) (a,b) -> map.Add(a,b)) Map.empty;;

// Point 3
let extendST sym decls = List.fold (fun (map:SymbolTable) (a,b) -> map.Add(a,b)) sym decls;;

type Exp =
  | V of string
  | A of string * Exp list;;

let expression = A (">", [V "x"; V "y"]);;
// Point 4
let rec symbolsDefined (sym: SymbolTable) (exp: Exp) =
  let rec aux (sym: SymbolTable) = function
    | V x -> not (sym.TryFind(x) = None)
    | A (x, ls) -> not (sym.TryFind(x) = None) && aux' ls
  and aux' = function
    | [] -> true
    | x :: tail -> aux sym x && aux' tail

  aux sym exp;;

// Point 5
let typOf (sym: SymbolTable) (exp: Exp) =
  let rec aux (sym: SymbolTable) x =
    match sym.TryFind(x) with
      | Some y -> y
      | None -> failwith "not strongly typed"

  match exp with
    | V x -> aux sym x
    | A (x, ls) -> aux sym x;;
  