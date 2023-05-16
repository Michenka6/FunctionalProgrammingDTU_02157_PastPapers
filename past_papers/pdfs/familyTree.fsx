type Sex =
    | M
    | F
// Problem 3 (40%)
type Name = string
type YearOfBirth = int
type FamilyTree = P of Name * Sex * YearOfBirth * Children

and Children = FamilyTree list

let peter = P("Peter", M, 2005, [])
let bob = P("Bob", M, 2008, [])
let eve = P("Eve", F, 2010, [])

let fred = P("Fred", M, 1970, [])
let joan = P("Joan", F, 1975, [])
let stanley = P("Stanley", M, 1975, [])
let mary = P("Mary", F, 1980, [ peter; bob; eve ])
let jane = P("Jane", F, 1985, [])

let may = P("May", F, 1945, [ fred; joan ])
let joe = P("Joe", M, 1950, [ stanley; mary; jane ])
let paul = P("Paul", M, 1955, [])

let larry = P("Larry", M, 1920, [ may; joe; paul ])

// Point 1
let getAge (P (_, _, x, _)) = x

let rec isWF (P (name, sex, year, children): FamilyTree) =
    match children with
    | [] -> true
    | _ ->
        year > (children |> List.maxBy getAge |> getAge)
        && children = List.sortByDescending getAge children

// Point 2
let makePerson (name, sex, year) = P(name, sex, year, [])

let getName (P (n, _, _, _)) = n

let rec insertChildOf name child (P (n, s, y, children)) =
    if n = name then
        let newChildren = child :: children |> List.sortByDescending getAge
        Some(P(n, s, y, newChildren))
    else
        match insertChildOfInList name child children with
        | Some c -> Some(P(n, s, y, c))
        | _ -> None

and insertChildOfInList name child children : Children option =
    let newChild =
        children |> List.map (insertChildOf name child) |> List.fold Option.orElse None

    match newChild with
    | None -> None
    | Some c ->
        let newChildren =
            List.map (fun p -> if (getName p) = (getName c) then c else p) children

        Some(newChildren)
