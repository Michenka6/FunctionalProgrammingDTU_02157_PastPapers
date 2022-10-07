// Problem 1 (30%)
type Appliance = string
type Usage = Appliance * int

let ad1: Usage = ("washing machine", 2)
let ad2: Usage = ("coffee machine", 1)
let ad3: Usage = ("dishwasher", 2)
let ats: Usage list = [ ad1; ad2; ad3; ad1; ad2 ]

// Point 1
let And = List.fold (&&) true

let inv (ls: Usage list) =
    ls |> List.map (fun (_, x) -> x > 0) |> And

// Point 2
let durationOf (a: Appliance) (ls: Usage list) =
    ls |> List.filter (fun (x, _) -> x = a) |> List.map snd |> List.sum

// Point 3
let wellFormed (ls: Usage list) =
    inv ls && ls |> List.map snd |> List.sum <= 24

// Point 4
let delete (a: Appliance) (ls: Usage list) =
    ls |> List.filter (fun (x, _) -> x <> a)

type Price = int
type Tariff = Map<Appliance, Price>

let trf =
    Map.empty
    |> Map.add "washing machine" 5
    |> Map.add "coffee machine" 1
    |> Map.add "dishwasher" 10

// Point 5
let isDefined (ats: Usage list) (trf: Tariff) =
    ats |> List.map (fun (x, _) -> trf |> Map.tryFind x <> None) |> And

// Point 6
let priceOf (ats: Usage list) (trf: Tariff) =
    ats |> List.map (fun (x, y) -> (trf |> Map.find x) * y) |> List.sum

// Problem 2 (35%)
let rec g1 p =
    function
    | x :: xs when p x -> x :: g1 p xs
    | _ -> []

let rec g2 f h n x =
    match n with
    | _ when n < 0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n - 1) (f x)

// Point 1
(*
    g1 : ('a -> bool) -> 'a list -> 'a list

    g2 : ('a -> 'a) -> ('a -> 'a) -> int -> 'a -> 'a
*)

// Point 2
let rec g1' acc p =
    function
    | x :: xs when p x -> g1' (x :: acc) p xs
    | _ -> List.rev acc

// Problem 3 (35%)
type Name = string
type Flow = int
type River = R of Name * Flow * Tributaries

and Tributaries = River list

// Point 1
let riv1 = R("R1", 5, [])
let riv3 = R("R3", 8, [])
let riv4 = R("R4", 2, [])
let riv2 = R("R2", 15, [ riv4 ])
let riv = R("R", 10, [ riv1; riv2; riv3 ])

// Point 2
let Or = List.fold (||) false

let rec contains n (R (x, y, xs): River) =
    x = n || xs |> List.map (contains n) |> Or

// Point 3
let rec allNames (R (x, y, xs): River) =
    [ x ] @ (xs |> List.map allNames |> List.concat)

// Point 4
let rec totalFlow (R (x, y, xs): River) =
    y + (xs |> List.map totalFlow |> List.sum)

// Point 5
let rec mainSource (riv: River) =
    let rec aux (R (x, y, xs): River) =
        [ (x, y) ] @ (xs |> List.map aux |> List.concat)

    aux riv |> List.sortBy snd |> List.last

// Point 6
let rec tryInsert n (t: River) (R (x, y, tr): River) =
    match tr with
    | ls when x = n -> Some(R(x, y, t :: ls))
    | head :: tail ->
        let d = (tryInsert n t head)
        if d = None then tryInsert n t (R(x, y, tail)) else d
    | _ -> None
