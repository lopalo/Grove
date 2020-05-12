module Grove.RTree

[<Struct>]
type Rectangle =
    {X : float
     Y : float
     X' : float
     Y' : float}

type IRectangular =
    abstract GetRect : unit -> Rectangle

type Node<'T when 'T :> IRectangular> =
    | Data of 'T
    | Node of Rectangle * subnodes : Nodes<'T>

and Nodes<'T when 'T :> IRectangular> = array<Node<'T>>

[<Struct>]
type InsertResult<'T when 'T :> IRectangular> =
    | Single of Node<'T>
    | Split of Node<'T> * Node<'T>

let minNodeSize = 2

let maxNodeSize = 4

let emptyRectangle =
    {X = 0.
     Y = 0.
     X' = 0.
     Y' = 0.}

let empty =
    Node(emptyRectangle, [||])

let mergeRectangles r r' =
    {X = min r.X r'.X
     Y = min r.Y r'.Y
     X' = max r.X' r'.X'
     Y' = max r.Y' r'.Y'}

let elementRect element =
    (element :> IRectangular).GetRect()

let nodeRect =
    function
    | Data el -> elementRect el
    | Node(r, _) -> r

let semiperimeter {X = x; Y = y; X' = x'; Y' = y'} =
    (x' - x) + (y' - y)

let insertCost element container =
    let s = semiperimeter container
    let s' = mergeRectangles container element |> semiperimeter
    struct (s' - s, s)

let isIdentical = LanguagePrimitives.PhysicalEquality

let splitNodes (nodes : Nodes<_>) =

    let pairs =
        [|for x in nodes do
            for y in nodes do
                if not (isIdentical x y) then struct (nodeRect x, nodeRect y)|]

    let struct (x, y) =
        Array.maxBy (fun (struct (x, y)) -> insertCost x y) pairs

    let (xNodes, yNodes) =
        Array.partition (fun n ->
            let r = nodeRect n
            insertCost r x < insertCost r y) nodes

    let rectOfNodes =
        Array.fold (fun rect n -> mergeRectangles rect (nodeRect n))
    let xRect = rectOfNodes x xNodes
    let yRect = rectOfNodes y yNodes
    Split(Node(xRect, xNodes), Node(yRect, yNodes))

let ensureMaxSize rectangle subnodes =
    if Array.length subnodes <= maxNodeSize then
        Single(Node(rectangle, subnodes))
    else
        splitNodes subnodes

let rec insert' newElement =
    function
    | Node(_, [||]) ->
        Single(Node(elementRect newElement, [|Data newElement|]))
    | Data el ->
        Split(Data el, Data newElement)
    | Node(rect, subnodes) ->
        let elRect = elementRect newElement

        let rect = mergeRectangles rect elRect
        let subnode = Array.minBy (nodeRect >> insertCost elRect) subnodes
        match insert' newElement subnode with
        | Single node ->
            let subnodes =
                Array.map
                    (fun n -> if isIdentical subnode n then node else n)
                    subnodes
            Single(Node(rect, subnodes))
        | Split(node, node') ->
            Array.filter (isIdentical subnode >> not) subnodes
            |> Array.append [|node; node'|]
            |> ensureMaxSize rect

let insert newElement root =
    match insert' newElement root with
    | Single root -> root
    | Split(node, node') ->
        let rect = mergeRectangles (nodeRect node) (nodeRect node')
        Node(rect, [|node; node'|])

type P =
    | P of x : float * y : float
    interface IRectangular with
        member this.GetRect() =
            let (P(x, y)) = this
            {X = x
             Y = y
             X' = x
             Y' = y}
(*
open Grove.RTree

let vals =
    [for x in 1.0 .. 12.0 do
        for y in 1.0 .. 12.0 do
            P(x, y)]
List.fold (fun x v -> insert v x) empty vals

vals
|> List.rev
|> List.fold (fun x v -> insert v x) empty

let r = System.Random()

let t =
    vals
    |> List.sortBy (fun _ -> r.Next(1000))
    |> List.fold (fun x v -> insert v x) empty

*)

//Helpers for testing in REPL
