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
    match act with
    | Some x -> x |> should equal 3
    | _ -> Assert.Fail()

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
    match act with
    | Some x -> Assert.That(x, Is.EqualTo(0.3).Within(0.01))
    | _ -> Assert.Fail()
    
[<Test>]
let ``String calculate should throw the error for incorrect strings`` () =
    let calculator = StringCalculateBuilder()
    calculator {
        let! x = "1"
        let! y = "a"
        let z = x + y
        return z
    }
    |> should equal None
    