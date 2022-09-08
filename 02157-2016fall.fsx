// Problem 1 (20%)
type Name = string;;
type Event = string;;
type Point = int;;
type Score = Name * Event * Point;;

type Scoreboard = Score list;;

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

// Point 1
let rec inv (sb:Scoreboard) =
  match sb with
  | [] | [_] -> true
  | (n, e, p) :: ((n1, e1, p1) :: tail) -> if p >= p1
                                            then inv ((n1, e1, p1) :: tail)
                                            else false;;

// Point 2
let rec insert ((n, e, p):Score) (sb:Scoreboard) =
  match sb with
  | [] -> [(n, e, p)]
  | (n1, e1, p1) :: tail -> if p >= p1
                              then (n, e, p) :: ((n1, e1, p1) :: tail)
                              else (n1, e1, p1) :: insert (n, e, p) tail;;

// Point 3
let rec get ((name:Name), (sb:Scoreboard)) =
  match sb with
  | [] -> failwith "Could not find!"
  | (n, e, p) :: tail -> if n = name
                          then (e, p)
                          else get (name, tail);;

// Point 4
let rec top k (sb:Scoreboard) =
  if k < 0 || k > (List.length sb)
    then None
    else Some (fst(sb |> List.splitAt k));;

// Problem 2 (15%)
// Point 1
let rec replace a b = function
  | [] -> []
  | head :: tail -> if head = a
                      then b :: replace a b tail
                      else a :: replace a b tail;;

// Point 2
(*
  replace : 'a -> 'a -> 'a list -> 'a list
*)

// Point 3
let rec replace' a b acc = function
  | [] -> acc
  | head :: tail -> if head = a
                      then replace' a b (acc @ [b]) tail
                      else replace' a b (acc @ [a]) tail;;

// Problem 3 (10%)
let pos = Seq.initInfinite (fun i -> i+1);;

let seq1 = seq { yield (0,0)
                 for i in pos do 
                 yield (i,i)
                 yield (-i,-i) }
                 
let val1 = Seq.take 5 seq1;;

let nat = Seq.initInfinite id;;

let seq2 = seq { for i in nat do
                 yield (i,0)
                 for j in [1 .. i] do
                 yield (i,j) };;

let val2 = Seq.toList(Seq.take 10 seq2);;