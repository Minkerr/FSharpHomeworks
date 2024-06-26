module Homework6.Tests.RoundingTest

open Homework6
open NUnit.Framework

[<Test>]
let ``Rounding 3 with accuracy equals should work for simple test`` () =
    let rounding = RoundingBuilder(3)
    let act =
        rounding {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }
    Assert.That(act, Is.EqualTo(0.048).Within(0.001))
    
[<Test>]
let ``Rounding with 0 accuracy should make result approximately integer`` () =
    let rounding = RoundingBuilder(0)
    let act =
        rounding {
            return 5.0 / 3.0
        }
    Assert.That(act, Is.EqualTo(2.0).Within(0.001))