module Homework4.Tests.PointFree

open FsCheck
open NUnit.Framework
open Homework4.PointFree

[<Test>]
let ```The point-free version of function and common version should work equally`` () =
    let isEqual x list = multiplyListByNumber x list = multiplyListByNumber'6 x list
    Check.QuickThrowOnFailure isEqual