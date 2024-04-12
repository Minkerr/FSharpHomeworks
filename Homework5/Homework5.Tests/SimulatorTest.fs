module Homework5.Tests

open NUnit.Framework
open FsUnit
open Simulator

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    let infectedComputer = Computer(Windows)
    infectedComputer.Infected <- true;
    let network =
        let computers = [
            infectedComputer
            Computer Linux
            Computer MacOS
            Computer Windows
            Computer Linux
        ]
        let connections = [[1]; [0; 2]; [1; 3]; [2; 4]; [3]]
        let virus = Virus(0.8, 0.2, 0.3)
        Network(computers, connections, virus)
    let infectedComputers = network.Simulate()
    List.length infectedComputers |> should equal 5