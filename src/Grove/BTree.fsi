module Grove.BTree

type private Node<'T when 'T : comparison> =
    | OneData of 'T
    | TwoData of 'T * 'T
    | TwoNode of Node<'T> * 'T * Node<'T>
    | ThreeNode of Node<'T> * 'T * Node<'T> * 'T * Node<'T>

[<Struct>]
type BTree<'T when 'T : comparison > =
    private
    | Empty
    | Filled of Node<'T>

val empty<'T when 'T : comparison> : BTree<'T>

val insert<'T when 'T : comparison> : newElement:'T -> tree:BTree<'T> -> BTree<'T>

