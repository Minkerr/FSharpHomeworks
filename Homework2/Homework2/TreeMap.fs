module TreeMap

type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Empty
    
let rec mapTree func tree =
    match tree with
    | Tree(a, l, r) -> Tree(func a, mapTree func l, mapTree func r)
    | Empty -> Empty

