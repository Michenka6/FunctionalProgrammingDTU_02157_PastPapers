// Problem 3
type Title = string;;

type Section = Title * Elem list
  and Elem =
    | Par of string
    | Sub of Section;;

type Chapter = Title * Section list;;
type Book = Chapter list;;

let sec11 = ("Background", [Par "bla";Sub(("Why programming", [Par "Bla."]))]);;
let sec12 = ("An example", [Par "bla"; Sub(("Special features", [Par "Bla."]))]);;
let sec21 = ("Fundamental concepts",[Par "bla"; Sub(("Mathematical background", [Par "Bla."]))]);;
let sec22 = ("Operational semantics",[Sub(("Basics", [Par "Bla."])); Sub(("Applications", [Par "Bla."]))]);;
let sec23 = ("Further reading",  [Par "bla"]);;
let sec31 = ("Overview", [Par "bla"]);;
let sec32 = ("A simple example", [Par "bla"]);;
let sec33 = ("An advanced example", [Par "bla"]);;
let sec41 = ("Status", [Par "bla"]);;
let sec42 = ("What's next?", [Par "bla"]);;
let ch1   = ("Introduction", [sec11; sec12]);;
let ch2   = ("Basic Issues", [sec21; sec22; sec23]);;
let ch3   = ("Advanced Issues", [sec31; sec32; sec33; sec23]);;
let ch4   = ("Conclusion", [sec41; sec42]);;
let book1 = [ch1; ch2; ch3; ch4];;

//  Point 1
let rec max x y = if x < y
                    then y
                    else x;;

let rec maxL = function
  | [] -> 0
  | [x] -> x
  | x :: tail -> max x (maxL tail);;

// Point 2
let rec overview = function
  | [] -> []
  | (x, _) :: tail -> x :: overview tail;;

// Point 3
type Numbering = int list;;
type Entry = Numbering * Title;;
type Toc = Entry list;;

// Point 4
let rec toc book1 =
  let rec sectionAux n m k (l, ls) =
      match ls with
        | [] -> []
        | Par x :: tail -> sectionAux n m k (l, tail)
        | Sub (x,_) :: tail -> ([n;m;k], x) :: sectionAux n m (k + 1) (l, tail)

  let rec chapterAux n m = function
    | (_, []) -> []
    | (x, (a,b) :: tail) -> (([n;m], a) :: sectionAux n m 1 (a, b)) @ chapterAux n (m + 1) (x, tail)

  let rec bookAux n = function
    | [] -> []
    | (x, xs) :: tail -> (([n], x) :: chapterAux n 1 (x, xs)) @ bookAux (n + 1) tail

  bookAux 1 book1;;

// Problem 1
let rec f xs ys =
  match (xs, ys) with
   | (x :: xs1, y :: ys1) -> x :: y :: f xs1 ys1
   | _ -> [];;

(*
  f [1;6;0;8] [0;7;3;3] =
  = 1 :: 0 :: f [6;0;8] [7;3;3] =
  = 1 :: 0 :: 6 :: 7 :: f [0;8] [3;3] =
  = 1 :: 0 :: 6 :: 7 :: 0 :: 3 :: f [8] [3] =
  = 1 :: 0 :: 6 :: 7 :: 0 :: 3 :: 8 :: 3 =
  = [1;0;6;7;0;3;8;3]
*)