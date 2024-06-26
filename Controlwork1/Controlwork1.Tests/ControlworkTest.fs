module Controlwork1.Tests

open NUnit.Framework
open Controlwork
open FsUnit

[<Test>]
let ``Test square with n = 1`` () =
    (square 1) |> should equal "*"
    
[<Test>]
let ``Test square with n = 2`` () =
    (square 2) |> should equal "**\n**"
    
[<Test>]
let ``Test square with n = 4`` () =
    (square 4) |> should equal "****\n*  *\n*  *\n****"
    
[<Test>]
let ``Test square with n = 5`` () =
    (square 5) |> should equal "*****\n*   *\n*   *\n*   *\n*****"
    
[<Test>]
let ``Test square with n = 0`` () =
    (square 0) |> should equal ""
    
[<Test>]
let ``Test square with n = -1`` () =
    (square -1) |> should equal ""
    
[<Test>]
let ``Test square with n = -3`` () =
    (square -3) |> should equal ""
    
[<Test>]
let ``Test sum of even fibonacci less than 1000000`` () =
    (sumEvenFibonacciLessThanMillion ()) |> should equal 1089154

[<Test>]
let ``Enqueue should add element to priority queue`` () =
    let queue = PriorityQueue()
    queue.Enqueue(("a", 3))
    queue.Enqueue(("b", 1))
    queue.Enqueue(("c", 2))
    queue.Peek() |> should equal "b"
    
[<Test>]
let ``Dequeue should remove element with the lowes priority from priority queue`` () =
    let queue = PriorityQueue()
    queue.Enqueue(("a", 3))
    queue.Enqueue(("b", 1))
    queue.Enqueue(("c", 2))
    queue.Dequeue() |> ignore
    queue.Dequeue() |> should equal "c"

[<Test>]
let ``Dequeue from empty priority queue should throw exception`` () =
    let queue = PriorityQueue()
    (fun () -> queue.Dequeue()) |> should throw typeof<System.Exception>
    
[<Test>]
let ``Peek from empty priority queue should throw exception`` () =
    let queue = PriorityQueue()
    (fun () -> queue.Peek()) |> should throw typeof<System.Exception>