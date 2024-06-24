module Homework4.Tests.PhoneBookTest

open Homework4
open NUnit.Framework
open PhoneBook
open FsUnit

[<Test>]
let ``Test adding to phonebook`` () =
    addRecord ("Bob", "123") [] |> should contain ("Bob", "123")
    
[<Test>]
let ``Test adding invalid value to phonebook`` () =
    addRecord ("Bob", "123") [] |> should contain ("Bob", "123")
    
[<Test>]
let ``Test finding phone by name`` () =
    let book = [("A", "12"); ("Bob", "123"); ("C", "13")]
    findPhoneByName "Bob" book |> should equal [("Bob", "123")]
    
[<Test>]
let ``Test finding duplicates`` () =
    let book = [("A", "12"); ("Bob", "123"); ("C", "13"); ("A", "121313")]
    findPhoneByName "A" book |> should equal [("A", "12"); ("A", "121313")]
    
[<Test>]
let ``Test finding phone by name when element is absent`` () =
    let book = addRecord ("Bob", "123") []
    findPhoneByName "Bo" book |> should equal []
    
[<Test>]
let ``Test finding name by phone`` () =
    let book = [("A", "12"); ("Bob", "123"); ("C", "13")]
    findNameByPhone "123" book |> should equal [("Bob", "123")]
    
[<Test>]
let ``Test finding name by phone when element is absent`` () =
    let book = addRecord ("Bob", "123") []
    List.length (findNameByPhone "12" book) |> should equal 0
    
[<Test>]
let ``Test writing and reading phonebook`` () =
    let path = "output.txt"
    let book = [("A", "12"); ("Bob", "123")]
    writePhoneBookToFile path book
    readPhoneBookFromFile path |> should equal [("A", "12"); ("Bob", "123")]
    
[<Test>]
let ``Test reading phonebook with invalid line`` () =
    let path = "input.txt"
    readPhoneBookFromFile path |> should equal [("Mike", "239"); ("Tommy", "666"); ("Simone", "58008")]