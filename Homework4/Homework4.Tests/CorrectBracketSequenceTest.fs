module Homework4.Tests.CorrectBracketSequenceTest

open Homework4
open NUnit.Framework
open CorrectBracketSequence
open FsUnit

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test the correctness of the string "()"`` () =
    (checkStringForCorrectBracketSequence "()") |> should equal true
    
[<Test>]
let ``Test the correctness of the string "aa(a[sa]{as}a)32rf"`` () =
    (checkStringForCorrectBracketSequence "aa(a[sa]{as}a)32rf") |> should equal true

[<Test>]
let ``Test the correctness of the string "("`` () =
    (checkStringForCorrectBracketSequence "(") |> should equal false

[<Test>]
let ``Test the correctness of the string "([)]"`` () =
    (checkStringForCorrectBracketSequence "([)]") |> should equal false
    