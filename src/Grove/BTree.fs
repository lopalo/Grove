module Grove.BTree

type IKeyable<'T when 'T : comparison> =
    abstract GetKey : unit -> 'T

type Node<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> =
    | OneData of 'T
    | TwoData of 'T * 'T
    | TwoNode of Node<'T, 'U> * 'T * Node<'T, 'U>
    | ThreeNode of Node<'T, 'U> * 'T * Node<'T, 'U> * 'T * Node<'T, 'U>


[<Struct>]
type BTree<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> =
    | Empty
    | Filled of Node<'T, 'U>

[<Struct>]
type InsertResult<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> =
    | Single of Node<'T, 'U>
    | Split of Node<'T, 'U> * 'T * Node<'T, 'U>

type RemoveResult<'T> = ValueOption<'T>

let empty<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> : BTree<'T, 'U> =
    Empty


let elementKey element =
    (element :> IKeyable<'T>).GetKey()

let elementEqual x y =
    elementKey x = elementKey y

let elementLessThan x y =
    elementKey x < elementKey y

let rec insert' newElement =
    function
    | OneData el ->
        Single
            (if elementEqual newElement el then
                OneData newElement
             else if elementLessThan newElement el then
                 TwoData(newElement, el)
             else
                 TwoData(el, newElement))
    | TwoData(leftEl, rightEl) ->
        if elementEqual newElement leftEl then
            Single(TwoData(newElement, rightEl))
        else if elementEqual newElement rightEl then
            Single(TwoData(leftEl, rightEl))
        else if elementLessThan newElement leftEl then
            Split(OneData newElement, leftEl, OneData rightEl)
        else if elementLessThan newElement rightEl then
            Split(OneData leftEl, newElement, OneData rightEl)
        else
            Split(OneData leftEl, rightEl, OneData newElement)
    | TwoNode(left, el, right) ->
        if elementEqual newElement el then
            Single(TwoNode(left, newElement, right))
        else if elementLessThan newElement el then
            match insert' newElement left with
            | Single left -> Single(TwoNode(left, el, right))
            | Split(left, leftEl, middle) ->
                Single(ThreeNode(left, leftEl, middle, el, right))
        else
            match insert' newElement right with
            | Single right -> Single(TwoNode(left, el, right))
            | Split(middle, rightEl, right) ->
                Single(ThreeNode(left, el, middle, rightEl, right))
    | ThreeNode(left, leftEl, middle, rightEl, right) ->
        if elementEqual newElement leftEl then
            Single(ThreeNode(left, newElement, middle, rightEl, right))
        else if elementEqual newElement rightEl then
            Single(ThreeNode(left, leftEl, middle, newElement, right))
        else if elementLessThan newElement leftEl then
            match insert' newElement left with
            | Single left ->
                Single(ThreeNode(left, leftEl, middle, rightEl, right))
            | Split(splitLeft, splitEl, splitRight) ->
                Split
                    (TwoNode(splitLeft, splitEl, splitRight), leftEl,
                     TwoNode(middle, rightEl, right))
        else if elementLessThan newElement rightEl then
            match insert' newElement middle with
            | Single middle ->
                Single(ThreeNode(left, leftEl, middle, rightEl, right))
            | Split(splitLeft, splitEl, splitRight) ->
                Split
                    (TwoNode(left, leftEl, splitLeft), splitEl,
                     TwoNode(splitRight, rightEl, right))
        else
            match insert' newElement right with
            | Single right ->
                Single(ThreeNode(left, leftEl, middle, rightEl, right))
            | Split(splitLeft, splitEl, splitRight) ->
                Split
                    (TwoNode(left, leftEl, middle), rightEl,
                     TwoNode(splitLeft, splitEl, splitRight))

let insert newElement =
    function
    | Empty -> Filled(OneData newElement)
    | Filled node ->
        Filled
            (match insert' newElement node with
             | Single tree -> tree
             | Split(left, element, right) -> TwoNode(left, element, right))


let rec searchByKey' key =
    function
    | OneData el ->
        if key = elementKey el then Some el else None
    | TwoData(leftEl, rightEl) ->
        if key = elementKey leftEl then Some leftEl
        else if key = elementKey rightEl then Some rightEl
        else None
    | TwoNode(left, el, right) ->
        let elKey = elementKey el
        if key = elKey then Some el
        else if key < elKey then searchByKey' key left
        else searchByKey' key right
    | ThreeNode(left, leftEl, middle, rightEl, right) ->
        let leftElKey = elementKey leftEl
        let rightElKey = elementKey rightEl
        if key = leftElKey then Some leftEl
        else if key = rightElKey then Some rightEl
        else if key < leftElKey then searchByKey' key left
        else if key < rightElKey then searchByKey' key middle
        else searchByKey' key right

let rec iterateFromKey' key =
    function
    | OneData el ->
        if key <= elementKey el then Seq.singleton el else Seq.empty
    | TwoData(leftEl, rightEl) ->
        seq {
            if key <= elementKey leftEl then yield leftEl
            if key <= elementKey rightEl then yield rightEl
        }
    | TwoNode(left, el, right) ->
        let elKey = elementKey el
        seq {
            if key < elKey then yield! iterateFromKey' key left
            if key <= elKey then yield el
            yield! iterateFromKey' key right
        }
    | ThreeNode(left, leftEl, middle, rightEl, right) ->
        let leftElKey = elementKey leftEl
        let rightElKey = elementKey rightEl
        seq {
            if key < leftElKey then yield! iterateFromKey' key left
            if key <= leftElKey then yield leftEl
            if key < rightElKey then yield! iterateFromKey' key middle
            if key <= rightElKey then yield rightEl
            yield! iterateFromKey' key right
        }

let searchByKey key =
    function
    | Empty -> None
    | Filled node -> searchByKey' key node

let iterateFromKey key =
    function
    | Empty -> Seq.empty
    | Filled node -> iterateFromKey' key node

let range startKey endKey tree =
    iterateFromKey startKey tree
    |> Seq.takeWhile (fun el -> elementKey el <= endKey)

type V<'T when 'T : comparison> =
    | V of 'T
    interface IKeyable<'T> with
        member this.GetKey() =
            let (V x) = this in x
(*
//Helpers for testing in REPL
open Grove.BTree
let vals = [for i in 1 .. 128 -> V i]
List.fold (fun x v -> insert v x) empty vals

vals
|> List.rev
|> List.fold (fun x v -> insert v x) empty

let r = System.Random()
let t =
    vals
    |> List.sortBy (fun _ -> r.Next(1000))
    |> List.fold (fun x v -> insert v x) empty

searchByKey 42 t, searchByKey 888 t
range -10 5 t |> List.ofSeq
range 15 20 t |> List.ofSeq
range 125 1000 t |> List.ofSeq
*)







//Helpers for testing in REPL
