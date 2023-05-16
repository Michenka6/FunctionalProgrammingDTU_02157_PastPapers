// Two (
//     2,
//     Two (
//         3,
//         One 0,
//         5,
//         One 1),
//     3,
//     Two (
//         2,
//         One 6,
//         3,
//         One 7),
// )


type T =
    | One of int
    | Two of int * T * int * T

type Trie<'a> = N of 'a * bool * Children<'a>
and Children<'a> = Trie<'a> list

let rec count (N (_, _, children)) = 1 + List.sumBy count children

let rec count' (N (a, b, children)) =
    match children with
    | [] -> 1
    | x :: xs -> count' x + count' (N(a, b, xs))



let t1 = N(0, false, [ N(0, false, [ N(1, true, []) ]) ])
let t2 = N(0, true, [ N(0, false, [ N(1, true, []) ]) ])
let ta = N(1, true, N(2, true, []) :: [])
let tb = N(3, false, N(0, true, []) :: [])
let tc = N(2, true, [])

let t3 = N(0, false, [ ta; tb; tc ])

// count N(1, true, N(2, true, []) :: [])
// => count (N(2, true, [])) + count (N(1, true, []))
// => 1 + 1
// => 2

// count tb
// => count (N(0, true, [])) + count (N(3, false, []))
// => 1 + 1
// => 2


// count N(0, false, [ ta; tb; tc ])

// => count N(1, true, N(2, true, []) :: [] ) + N (0,false, [ tb; tc ])

// => count N(2, true, []) + count N(1, true, []) + N (0,false, [ tb; tc ])

// => 1 + 1 + count tb + count N (0,false, [tc])
// => 2 + 2 + count tc + count N (0,false, [])
// => 2 + 2 + 1 + 1
// => 6


let rec accept w (N (a, b, children)) =
    match w with
    | [] -> false
    | [ x ] -> x = a && b
    | x :: xs when x = a -> List.exists (fun child -> accept xs child) children
    | _ -> false

// accept : int list -> Trie<int> -> bool

type Word = int list

let rec wordsOf (N (a, b, children)) : Set<Word> =
    let aux children =
        children
        |> List.map wordsOf
        |> List.map (Set.map (fun word -> a :: word))
        |> Set.unionMany

    if b then Set [ [ a ] ] + aux children else aux children

let rec uselessLeaves (N (_, b, children)) =
    match children with
    | [] when b -> false
    | [] -> true
    | _ -> List.exists uselessLeaves children

let rec degree (N (_, _, children)) =
    children
    |> List.map degree
    |> List.max
    |> fun v -> max (List.length children) v


// let ta = N(1, true, N(2, true, []) :: [])
// let tb = N(3, false, N(0, true, []) :: [])
// let tc = N(2, true, [])

// let t3 = N(0, false, [ ta; tb; tc ])

// degree ta
// => degree N(1, true, N(2, true, []) :: [])
// => degree N(1, true, degree (N(2, true, [])) :: [])
// => degree N(1, true, 0 :: [])
// => max 1 0
// => 1

// degree t3
// => degree N(0, false, [ ta; tb; tc ])
// => degree N(0, false, [ degree ta; degree tb; degree tc ])
// => degree N(0, false, [ 1; 1; 0])
// => max 3 1
// => 3
