module Homework4.Tests
open FsCheck
open FsUnit
open NUnit.Framework
open Homework4.PointFree

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ```The point-free version of function and common version should work equally`` () =
    let isEqual x list = multiplyListByNumber x list = multiplyListByNumber'5 x list
    Check.QuickThrowOnFailure isEqual