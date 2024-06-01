module Homework7.Tests.MiniCrawler

open Homework7.MiniCrawler
open NUnit.Framework
open FsUnit
open Moq
open HtmlAgilityPack

[<Test>]
let ``MiniCrawler Test`` () =
    (crawler "https://example.com/") |> should equal ["https://www.iana.org/domains/example", Some 73]
    
[<Test>]
let mock = Mock<HtmlWeb>()

//mock.Setup(fun client -> client.Load("hwproj.ru"))
  //.ReturnsAsync("<html>нормальная ссылка / битая ссылка</html>");
          
//downloadPages "hwproj.ru" mock.Object