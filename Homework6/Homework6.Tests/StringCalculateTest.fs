module Homework6.Tests.StringCalculateTest

open Homework6
open NUnit.Framework
open FsUnit

[<Test>]
let ``String calculate should work with simple addition`` () =
    let calculator = StringCalculateBuilder()
    let act =
        calculator {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }
    act |> should equal 3

[<Test>]
let ``String calculate should work with double values`` () =
    let calculator = StringCalculateBuilder()
    let act =
        calculator {
            let! x = "-0,6"
            let! y = "-2"
            let z = x / y
            return z
        }
    act - 0.001 |> should lessThan 0.3
    act + 0.001 |> should greaterThan 0.3
    
[<Test>]
let ``String calculate should throw the error for incorrect strings`` () =
    let calculator = StringCalculateBuilder()
    try
        calculator {
            let! x = "1"
            let! y = "a"
            let z = x + y
            return z
        } |> ignore
    with ex ->
        ex.Message |> should equal "IncorrectInputString \"String should be a number\""
        
    
    //|> should throw typeof<IncorrectInputString>