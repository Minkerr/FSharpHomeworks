module Controlwork1.Tests

open NUnit.Framework
open Controlwork
open FsUnit

[<Test>]
let ``Test square with n = 1`` () =
    (square 1) |> should equal "*"
    
[<Test>]
let ``Test square with n = 2`` () =
    (square 2) |> should equal "**\n**"
    
[<Test>]
let ``Test square with n = 4`` () =
    (square 4) |> should equal "****\n*  *\n*  *\n****"
    
[<Test>]
let ``Test square with n = 0`` () =
    (square 0) |> should equal ""
    
[<Test>]
let ``Test sum of even fibonacci less than 1000000`` () =
    (sumEvenFibonacciLessThanMillion ()) |> should equal 1089154
