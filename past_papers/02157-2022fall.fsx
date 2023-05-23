// Problem 1
type Team = string
type Goal = int
type Point = int
type Matches = int
type Score = Matches * Goal * Goal * Point
type TeamScore = Team * Score
type Standings = TeamScore list
type MatchResult = Team * Team * Goal * Goal

let ss =
    [ ("T1", (3, 5, 1, 9))
      ("T2", (3, 4, 2, 4))
      ("T3", (3, 4, 4, 4))
      ("T4", (3, 0, 6, 0)) ]

// Point 1
let better (m1, gs1, gc1, p1) (m2, gs2, gc2, p2) =
    p1 > p2 || (p1 = p2 && gs1 - gc1 > gs2 - gc2)

let equal (m1, gs1, gc1, p1) (m2, gs2, gc2, p2) = p1 = p2 && gs1 - gc1 = gs2 - gc2

let betterOrEqual sc1 sc2 = better sc1 sc2 || equal sc1 sc2

// Point 2
let rec properlyOrdered =
    function
    | [] -> true
    | [ _ ] -> true
    | x :: y :: tail -> betterOrEqual x y && properlyOrdered (y :: tail)

// Point 3
let initialScore = (0, 0, 0, 0)

let rec init =
    function
    | [] -> []
    | x :: xs -> (x, initialScore) :: init xs

// Point 4
let rec tryFindTeam t =
    function
    | [] -> None
    | (team, score) :: tail when t = team -> Some score
    | _ :: tail -> tryFindTeam t tail

let rec deleteTeam t =
    function
    | [] -> []
    | (team, _) :: tail when t = team -> tail
    | head :: tail -> head :: deleteTeam t tail

let extractScoreOf t standings =
    match tryFindTeam t standings with
    | None -> failwith "Can't find the team"
    | Some score -> (score, deleteTeam t standings)

// Point 5
let update (m, gs, gc, p) g1 g2 =
    let pointsGained =
        if g1 < g2 then 0
        else if g1 = g2 then 1
        else 3

    (m + 1, gs + g1, gc + g2, p + pointsGained)

// Point 6
let rec insertTeamScore (t, sc) =
    function
    | [] -> [ (t, sc) ]
    | (team, score) :: tail when betterOrEqual sc score -> (t, sc) :: (team, score) :: tail
    | head :: tail -> head :: insertTeamScore (t, sc) tail

// Point 7
let newStandings (t1, t2, g1, g2) ss =
    let oldSc1, ss1 = extractScoreOf t1 ss
    let newSc1 = update oldSc1 g1 g2

    let oldSc2, ss2 = extractScoreOf t2 ss1
    let newSc2 = update oldSc2 g2 g1


    ss2 |> insertTeamScore (t1, newSc1) |> insertTeamScore (t2, newSc2)


// Problem 2
let rec partition p =
    function
    | [] -> [], []
    | x :: rest ->
        let xs1, xs2 = partition p rest
        if p x then x :: xs1, xs2 else xs1, x :: xs2

// Point 1
(*
    partition: ('a -> bool) -> 'a list -> 'a list * 'a list
*)

(*
    let ls = [0;1;2]
    let isEven x = x % 2 = 0

    partition isEven [2] =
    => 2 :: [] -> 
            let xs1, xs2 = partition isEven []
                         = [], []
            if isEven 2 then 2 :: [], []

    partition isEven [1;2]
    => 1 :: [2]  -> 
            let xs1,xs2 = partition isEven [2]
                        = [2], []

            if isEven 1 then ###
            else [2], 1 :: []
    
    partition isEven [0;1;2]
    => 0 :: [1;2] -> 
            let xs1,xs2 = partition isEven [1,2]
                        = [2], [1]

            if isEven 0 then 0 :: [2], [1]

    => ([0; 2], [1])
*)

// Point 2
let a x = x > 5
let b = [ 1; 2; 4; 6; 8; 9 ]

// Point 3
let rec partitionACC p (xs1, xs2) =
    function
    | [] -> List.rev xs1, List.rev xs2
    | x :: rest ->
        if p x then
            partitionACC p (x :: xs1, xs2) rest
        else
            partitionACC p (xs1, x :: xs2) rest

// Point 4
let partitionFoldBack p xs =
    List.foldBack (fun x (xs1, xs2) -> if p x then x :: xs1, xs2 else xs1, x :: xs2) xs ([], [])

// Problem 3
type H<'d> = N of 'd * Children<'d>
and Children<'d> = H<'d> list

// Point 1
let h =
    let c = N(4, [])
    let a = N(2, [ c ])
    let b = N(3, [])
    N(1, [ a; b ])

// Point 2
let rec descriptionOf (N (d, children)) =
    List.fold (fun acc x -> acc @ descriptionOf x) [ d ] children


let rec dOf acc (N (d, c)) =
    match c with
    | [] -> List.rev (d :: acc)
    | x :: xs -> dOf (dOf acc x) (N(d, xs))

let rec descriptionOf' (N (d, c)) =
    match c with
    | [] -> [ d ]
    | x :: xs -> descriptionOf' x @ descriptionOf' (N(d, xs))

(*
    descriptionOf' c
    => [ 4 ]

    descriptionOf' a
    => descriptionOf' c @ descriptionOf' (N (2,[]))
    => [ 4 ] @ [ 2 ]
    => [ 4; 2 ]

    descriptionOf' b
    => [ 3 ]

    desriptionOf' (N (1, [ b ])
    => descriptionOf' b @ descriptionOf' (N (1, []))
    => [ 3 ] @ [ 1 ]
    => [ 3; 1 ]

    descriptionOf' h
    => descriptionOf' a @ desriptionOf' (N (1, [ b ]))
    => [ 4; 2 ] @ [ 3; 1 ]
    => [ 4; 2; 3; 1 ]
*)

let rec mapH f (N (d, c)) = N(f d, List.map (mapH f) c)

let rec mapH' f (N (d, c)) =
    match c with
    | [] -> N(f d, [])
    | x :: xs ->
        let x' = mapH' f x
        let (N (d', xs')) = mapH' f (N(d, xs))
        N(d', x' :: xs')

// Point 4
let org: H<string * int> =
    let b1 = N(("B1", 3), [])
    let b2 = N(("B2", 5), [])
    let l = N(("L", 4), [])
    let m = N(("M", 4), [ b1; b2 ])
    let r = N(("R", 2), [])

    N(("Top", 5), [ l; m; r ])

// Point 5
let rec numberOf (N ((_, n), c)) = n + List.sumBy numberOf c

let rec numberOf' (N ((t, n), c)) =
    match c with
    | [] -> n
    | x :: xs -> numberOf' x + numberOf' (N((t, n), xs))

let rec numberOf'' (h: H<string * int>) = h |> descriptionOf |> List.sumBy snd

// Point 6
let largest org =
    let _, m = List.maxBy snd (descriptionOf org)
    let ts = descriptionOf org |> List.filter (fun (t, n) -> n = m) |> List.map fst
    ts, m

// Problem 4
// List.sumBy
let rec f g xs =
    match xs with
    | [] -> 0
    | x :: tail -> g x + f g tail

let h' (x, y) = x + y + 1

// Problem 5
let nat = Seq.initInfinite id

let nat3 = Seq.map (fun x -> 3 * x, 3 * x + 1, 3 * x + 2) nat

let nat3' =
    seq {
        for x in nat do
            yield 3 * x, 3 * x + 1, 3 * x + 2
    }
