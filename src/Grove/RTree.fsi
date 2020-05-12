module Grove.RTree

[<Struct>]
type Rectangle =
    {X: float
     Y: float
     X': float
     Y': float}

type IRectangular =
  interface
    abstract member GetRect : unit -> Rectangle
  end


type Node<'T when 'T :> IRectangular> =
    private
    | Data of 'T
    | Node of Rectangle * subnodes: Nodes<'T>

and private Nodes<'T when 'T :> IRectangular> = Node<'T> array

val empty : Node<#IRectangular>

val insert : newElement:'T -> root:Node<'T> -> Node<'T> when 'T :> IRectangular

val range : queryBox:Rectangle -> root:Node<'T> -> seq<'T> when 'T :> IRectangular

type P =
    | P of x: float * y: float
    interface IRectangular
