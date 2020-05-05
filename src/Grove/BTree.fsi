module Grove.BTree


type IKeyable<'T when 'T : comparison> =
    abstract GetKey : unit -> 'T

type private Node<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> =
    | OneData of 'T
    | TwoData of 'T * 'T
    | TwoNode of Node<'T, 'U> * 'T * Node<'T, 'U>
    | ThreeNode of Node<'T, 'U> * 'T * Node<'T, 'U> * 'T * Node<'T, 'U>


[<Struct>]
type BTree<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> =
    private
    | Empty
    | Filled of Node<'T, 'U>

val empty<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison> : BTree<'T, 'U>

val insert<'T, 'U when 'T :> IKeyable<'U> and 'U : comparison>
    : newElement:'T -> tree:BTree<'T, 'U> -> BTree<'T, 'U>

val searchByKey<'U, 'T when 'T :> IKeyable<'U> and 'U : comparison>
    : key:'U -> tree:BTree<'T,'U> -> Option<'T>

val iterateFromKey<'U, 'T when 'T :> IKeyable<'U> and 'U : comparison>
    : key:'U -> tree:BTree<'T,'U> -> seq<'T>

val range<'U, 'T when 'T :> IKeyable<'U> and 'U : comparison>
    : startKey:'U -> endKey:'U -> tree:BTree<'T,'U> -> seq<'T>

type V<'T when 'T : comparison> =
    | V of 'T
    interface IKeyable<'T>

