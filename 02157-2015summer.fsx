// Problem 1 (20%)
// Point 1
let rec repeat s = function
  | 0 -> ""
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n -> s + repeat s (n - 1);;

// Point 2
let rec f s1 s2 = function
  | 0 -> ""
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n when (n%2 = 0) -> s1 + "\n" + f s1 s2 (n - 1)
  | n -> s2 + "\n" + f s1 s2 (n - 1);;

// Point 3
let rec viz m = function
  | 0 -> ""
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n when (n%2 = 1) -> repeat "XO" m + "\n" + viz m (n - 1)
  | n -> repeat "OX" m + "\n" + viz m (n - 1);;
  
// Point 4
let rec repeat' (s: string) (acc: string) = function
  | 0 -> acc
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n -> repeat' s (acc + s) (n - 1);;

// Problem 2 (20%)
// Point 1
// Problem 1 (20%)
// Point 1
let rec repeat s = function
  | 0 -> ""
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n -> s + repeat s (n - 1);;

// Point 2
let rec f s1 s2 = function
  | 0 -> ""
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n when (n%2 = 0) -> s1 + "\n" + f s1 s2 (n - 1)
  | n -> s2 + "\n" + f s1 s2 (n - 1);;

// Point 3
let rec viz m = function
  | 0 -> ""
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n when (n%2 = 1) -> repeat "XO" m + "\n" + viz m (n - 1)
  | n -> repeat "OX" m + "\n" + viz m (n - 1);;
  
// Point 4
let rec repeat' (s: string) (acc: string) = function
  | 0 -> acc
  | n when n < 0 -> failwith "Can't repeat a negative number of times!"
  | n -> repeat' s (acc + s) (n - 1);;

// Problem 2 (20%)
// Point 1
let rec mixMap f = function
  | ([], []) -> []
  | (x :: xx, y :: yy) -> (f x, f y) :: mixMap f (xx, yy)
  | _ -> failwith "Can't mixMap unequal lists!";;

// Point 2
let rec unmixMap f g ls =
  let rec aux f g (a,b) = function
    | [] -> (List.rev a, List.rev b)
    | (a1,b1) :: xs -> aux f g (f a1 :: a, g b1 :: b) xs

  aux f g ([], []) ls;;

//