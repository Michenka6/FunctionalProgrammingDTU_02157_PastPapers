type Tree<'a> =
    | Lf of 'a
    | Br of Tree<'a> * 'a * Tree<'a>


let t = Br(Br(Lf 4, 2, Lf 5), 1, Br(Lf 6, 3, Lf 7))

let rec preOrder =
    function
    | Lf a -> [ a ]
    | Br (t1, a, t2) ->
        let left = preOrder t1
        let right = preOrder t2
        [ a ] @ left @ right

let rec inOrder =
    function
    | Lf a -> [ a ]
    | Br (t1, a, t2) ->
        let left = inOrder t1
        let right = inOrder t2
        left @ [ a ] @ right

let rec postOrder =
    function
    | Lf a -> [ a ]
    | Br (t1, a, t2) ->
        let left = postOrder t1
        let right = postOrder t2
        left @ right @ [ a ]

let rec preOrderACC acc =
    function
    | Lf a -> acc @ [ a ]
    | Br (t1, a, t2) ->
        let left = preOrderACC (acc @ [ a ]) t1
        preOrderACC left t2

let rec inOrderACC acc =
    function
    | Lf a -> acc @ [ a ]
    | Br (t1, a, t2) ->
        let left = inOrderACC acc t1
        inOrderACC (left @ [ a ]) t2

let rec postOrderACC acc =
    function
    | Lf a -> acc @ [ a ]
    | Br (t1, a, t2) ->
        let left = postOrderACC acc t1
        let right = postOrderACC left t2
        right @ [ a ]


let rec breadthFirst =
    function
    | [] -> []
    | t :: rest ->
        match t with
        | Lf a -> a :: breadthFirst rest
        | Br (t1, a, t2) -> a :: breadthFirst (rest @ [ t1; t2 ])
