type BinTree<'a> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

let rec depth =
    function
    | Leaf -> 0
    | Node (t1, _, t2) -> 1 + max (depth t1) (depth t2)

let rec preOrder =
    function
    | Leaf -> []
    | Node (tl, x, tr) -> x :: (preOrder tl) @ (preOrder tr)

let rec inOrder =
    function
    | Leaf -> []
    | Node (tl, x, tr) -> (inOrder tl) @ [ x ] @ (inOrder tr)

let rec postOrder =
    function
    | Leaf -> []
    | Node (tl, x, tr) -> (postOrder tl) @ (postOrder tr) @ [ x ]

let t3 = Node(Node(Leaf, -3, Leaf), 2, Node(Leaf, 2, Leaf))
let t4 = Node(t3, 5, Node(Leaf, 7, Leaf))

let preFold f acc tree = List.fold f acc (preOrder tree)

let preFoldBack f acc tree = List.foldBack f (preOrder tree) acc

let rec inFold f acc tree =
    match tree with
    | Leaf -> acc
    | Node (tl, x, tr) ->
        let el = inFold f acc tl
        let ex = f x el
        inFold f ex tr

let rec inFoldBack f acc tree =
    match tree with
    | Leaf -> acc
    | Node (tl, x, tr) ->
        let er = inFoldBack f acc tr
        let ex = f x er
        inFoldBack f ex tl

let inFold' f acc tree = List.fold f acc (inOrder tree)

let inFoldBack' f acc tree = List.foldBack f (inOrder tree) acc

let rec postFold f acc tree =
    match tree with
    | Leaf -> acc
    | Node (tl, x, tr) ->
        let el = postFold f acc tl
        let er = postFold f el tr
        f x er

let rec postFoldBack f acc tree =
    match tree with
    | Leaf -> acc
    | Node (tl, x, tr) ->
        let ex = f x acc
        let er = postFoldBack f ex tr
        postFoldBack f er tl

let postFold' f acc tree = List.fold f acc (postOrder tree)

let postFoldBack' f acc tree = List.foldBack f (postOrder tree) acc


type Circuit<'a> =
    | Comp of 'a
    | Ser of Circuit<'a> * Circuit<'a>
    | Par of Circuit<'a> * Circuit<'a>

let rec count =
    function
    | Comp _ -> 1
    | Ser (c1, c2) -> count c1 + count c2
    | Par (c1, c2) -> count c1 + count c2

let rec circRec (c, s, p) =
    function
    | Comp x -> c x
    | Ser (c1, c2) -> s (circRec (c, s, p) c1) (circRec (c, s, p) c2)
    | Par (c1, c2) -> p (circRec (c, s, p) c1) (circRec (c, s, p) c2)
