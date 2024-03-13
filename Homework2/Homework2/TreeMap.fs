module TreeMap

type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a
    | None
    
let rec mapTree func tree =
    match tree with
    | Tree(a, l, r) -> Tree(func a, mapTree func l, mapTree func r)
    | Tip(a) -> Tip(func a) 
    | None -> None

