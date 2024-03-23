module PrimeNumbersTest

open PrimeNumbers
open NUnit.Framework
open FsUnit

[<Test>]
let ``The sequence of 0 numbers should be empty`` () =
    Seq.take 0 primeNumbers |> should equal []

[<Test>]
let ``The first prime number should be 2`` () =
    Seq.take 1 primeNumbers |> should equal [2]

[<Test>]
let ``The first 2 prime numbers should be 2 and 3`` () =
    Seq.take 2 primeNumbers |> should equal [2; 3]
    
[<Test>]
let ``The first 5 prime numbers should be 2, 3, 5, 7, 11`` () =
    Seq.take 5 primeNumbers |> should equal [2; 3; 5; 7; 11]
