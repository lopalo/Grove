module Grove.BTree

type Node<'T when 'T : comparison> =
    | OneData of 'T
    | TwoData of 'T * 'T
    | TwoNode of Node<'T> * 'T * Node<'T>
    | ThreeNode of Node<'T> * 'T * Node<'T> * 'T * Node<'T>


[<Struct>]
type BTree<'T when 'T : comparison> =
    | Empty
    | Filled of Node<'T>

[<Struct>]
type InsertResult<'T when 'T : comparison> =
    | Single of Node<'T>
    | Split of Node<'T> * 'T * Node<'T>

type RemoveResult<'T> = ValueOption<'T>

let empty<'T when 'T : comparison> : BTree<'T> = Empty

let rec insert' newElement =
    function
    | OneData el ->
        Single
            (if newElement = el then OneData newElement
             else if newElement < el then TwoData(newElement, el)
             else TwoData(el, newElement))
    | TwoData(leftEl, rightEl) ->
        if newElement = leftEl then
            Single(TwoData(newElement, rightEl))
        else if newElement = rightEl then
            Single(TwoData(leftEl, rightEl))
        else if newElement < leftEl then
            Split(OneData newElement, leftEl, OneData rightEl)
        else if newElement < rightEl then
            Split(OneData leftEl, newElement, OneData rightEl)
        else
            Split(OneData leftEl, rightEl, OneData newElement)
    | TwoNode(left, el, right) ->
        if newElement = el then
            Single(TwoNode(left, newElement, right))
        else if newElement < el then
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
        if newElement = leftEl then
            Single(ThreeNode(left, newElement, middle, rightEl, right))
        else if newElement = rightEl then
            Single(ThreeNode(left, leftEl, middle, newElement, right))
        else if newElement < leftEl then
            match insert' newElement left with
            | Single left ->
                Single(ThreeNode(left, leftEl, middle, rightEl, right))
            | Split(splitLeft, splitEl, splitRight) ->
                Split
                    (TwoNode(splitLeft, splitEl, splitRight), leftEl,
                     TwoNode(middle, rightEl, right))
        else if newElement < rightEl then
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

//Helpers for testing in REPL
(*
open Grove.BTree
List.fold (fun x v -> insert v x) empty [1 .. 128]

let r = System.Random()

([1 .. 128]
 |> List.sortBy (fun _ -> r.Next(1000))
 |> List.fold (fun x v -> insert v x) empty)
*)
