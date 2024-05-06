module Homework7.Tests.LazyTest

open Homework7.Lazy
open NUnit.Framework
open FsUnit

[<Test>]
let ``Lazy Test``  () =
    let myILazy = Lazy(fun () -> 1) :> ILazy<int>
    myILazy.Get() |> should equal 1