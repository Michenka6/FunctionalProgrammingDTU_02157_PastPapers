// Problem 1 (30%)
let rec f n = function
  | 0          -> 1
  | k when k>0 -> n * (f n (k-1))
  | _          -> failwith "illegal argument";;

let rec g p f = function
  | []             -> []
  | x::xs when p x -> f x :: g p f xs
  | _::xs          -> g p f xs;;

type T = 
  | A of int
  | B of string
  | C of T * T;;

let rec h = function
  | A n      -> string n
  | B s      -> s
  | C(t1,t2) -> h t1 + h t2;;

let sq = Seq.initInfinite (fun i -> 3*i);;

let k j = seq {for i in sq doyield (i,i-j) };;

let xs = Seq.toList (Seq.take 4 sq);;

let ys = Seq.toList (Seq.take 4 (k 2));;

// Point 1
let p11 = f 4 3;;

let p12 = g (fun x -> x > 0) f [2;3;-1];;

let p13 = h (C (A 2, B " what "));;

// Point 2

// Point 3
