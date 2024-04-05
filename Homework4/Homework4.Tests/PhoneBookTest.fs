module Homework4.Tests.PhoneBookTest

open Homework4
open NUnit.Framework
open PhoneBook
open FsUnit

[<Test>]
let ``Test adding to phonebook`` () =
    addRecord ("Bob", "123") [] |> should contain ("Bob", "123")
    
[<Test>]
let ``Test finding phone by name`` () =
    let book = addRecord ("Bob", "123") []
    findPhoneByName "Bob" book |> should equal (Some("123"))
    
[<Test>]
let ``Test finding phone by name when element is absent`` () =
    let book = addRecord ("Bob", "123") []
    findPhoneByName "Bo" book |> should equal None
    
[<Test>]
let ``Test finding name by phone`` () =
    let book = addRecord ("Bob", "123") []
    findNameByPhone "123" book |> should equal (Some("Bob"))
    
[<Test>]
let ``Test finding name by phone when element is absent`` () =
    let book = addRecord ("Bob", "123") []
    findNameByPhone "12" book |> should equal None
    
