module TreeMapTest

open NUnit.Framework
open FsUnit
open TreeMap

[<Test>]
let ``Map empty tree should return empty tree`` () =
    Empty |> mapTree (fun x -> x) |> should equal Empty
    
[<Test>]
let ``Map Tip tree should return Tip(map(2))`` () =
    Tree(2, Empty, Empty) |> mapTree (fun x -> x * x) |> should equal (Tree(4, Empty, Empty))
    
[<Test>]
let ``Map Tree(2, Empty, Empty) tree should return Tree(map(2), Empty, Empty)`` () =
    let exp = Tree(4, Tree(9, Empty, Empty), Tree(1, Empty, Empty))
    let act = Tree(2, Tree(3, Empty, Empty), Tree(1, Empty, Empty)) |>
              mapTree (fun x -> x * x)
    act |> should equal exp
    
[<Test>]
let ``Map big tree should return tree with all nodes mapped`` () =
    let exp = Tree(1,
                   Tree(4, Empty, Tree(9, Tree(16, Empty, Empty), Empty)),
                   Tree(25, Tree(36, Empty, Empty), Tree(49, Empty, Empty)))
    let act = Tree(1,
                   Tree(2, Empty, Tree(3, Tree(4, Empty, Empty) , Empty)),
                   Tree(5, Tree(6, Empty, Empty) , Tree(7, Empty, Empty)))
              |> mapTree (fun x -> x * x)
    act |> should equal exp
    