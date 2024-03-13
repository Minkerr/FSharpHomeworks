module TreeMapTest

open NUnit.Framework
open FsUnit
open TreeMap

[<Test>]
let ``Map empty tree should return empty tree`` () =
    None |> mapTree (fun x -> x) |> should equal None
    
[<Test>]
let ``Map Tip tree should return Tip(map(2))`` () =
    Tip(2) |> mapTree (fun x -> x * x) |> should equal (Tip(4))
    
[<Test>]
let ``Map Tree(2, None, None) tree should return Tree(map(2), None, None)`` () =
    let exp = Tree(4, Tip(9), Tip(1))
    let act = Tree(2, Tip(3), Tip(1)) |> mapTree (fun x -> x * x)
    act |> should equal exp
    
[<Test>]
let ``Map big tree should return tree with all nodes mapped`` () =
    let exp = Tree(1, Tree(4, None, Tree(9, Tip(16), None)), Tree(25, Tip(36), Tip(49)))
    let act = Tree(1, Tree(2, None, Tree(3, Tip(4) , None)), Tree(5 , Tip(6) , Tip(7))) |> mapTree (fun x -> x * x)
    act |> should equal exp
    