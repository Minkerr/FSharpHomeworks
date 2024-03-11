module Homework2.Tests.CountEven

open CountEven
open NUnit.Framework
open FsUnit
open FsCheck

[<Test>]
let ``The number of even numbers in [1; 2; 3; 4; 5] is 2 by using Seq.filter`` () =
    countEvenFilter [1; 2; 3; 4; 5] |> should equal 2
    
[<Test>]
let ``The number of even numbers in [1; 2; 3; 4; 5] is 2 by using Seq.fold`` () =
    countEvenFold [1; 2; 3; 4; 5] |> should equal 2
    
[<Test>]
let ``The number of even numbers in [1; 2; 3; 4; 5] is 2 by using Seq.map`` () =
    (countEvenMap [1; 2; 3; 4; 5]) |> should equal 2
    
[<Test>]
let ``Quick equivalence check of fold and filter realization`` () =
    let isEqual (list : List<int>) = countEvenFilter list = countEvenFold list
    Check.QuickThrowOnFailure isEqual
    
[<Test>]
let ``Quick check of map and filter realization`` () =
    let isEqual (list : List<int>) = countEvenFilter list = countEvenMap list
    Check.QuickThrowOnFailure isEqual
    