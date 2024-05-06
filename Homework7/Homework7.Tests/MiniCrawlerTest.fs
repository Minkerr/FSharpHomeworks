module Homework7.Tests.MiniCrawler

open Homework7.MiniCrawler
open NUnit.Framework
open FsUnit

[<Test>]
let ``MiniCrawler Test``  () =
    (crawler "https://example.com/") |> should equal [73]