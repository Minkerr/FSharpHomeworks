module PrepositionCalculateTest

open NUnit.Framework
open FsUnit
open PrepositionCalculate

[<Test>]
let ``The value of Number(1) preposition tree is 1`` () =
    Number(1) |> calculate |> should equal 1
    
[<Test>]
let ``The value of Add(Number(2), Number(2)) preposition tree is 4`` () =
    Add(Number(2), Number(2)) |> calculate |> should equal 4
    
[<Test>]
let ``The value of big preposition tree is `` () =
    let act = Multiply(Subtract(Number(12), Number(1)), Divide(Number(15), Number(5))) |> calculate
    act |> should equal 33