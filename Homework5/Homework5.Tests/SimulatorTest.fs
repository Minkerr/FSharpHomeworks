module Homework5.Tests

open NUnit.Framework
open FsUnit
open Simulator

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test simulation for bamboo graph`` () =
    let infectedComputer = Computer Windows
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
    
[<Test>]
let ``Test simulation for sheaf graph`` () =
    let infectedComputer = Computer Windows
    infectedComputer.Infected <- true;
    let network =
        let computers = [
            infectedComputer
            Computer Linux
            Computer MacOS
            Computer Windows
            Computer Linux
        ]
        let connections = [[1; 2; 3; 4]; [0]; [0]; [0]; [0]]
        let virus = Virus(0.8, 0.2, 0.3)
        Network(computers, connections, virus)
    let infectedComputers = network.Simulate()
    List.length infectedComputers |> should equal 5
    
[<Test>]
let ``Test simulation for graph with isolated vertex`` () =
    let infectedComputer = Computer Windows
    infectedComputer.Infected <- true;
    let network =
        let computers = [
            infectedComputer
            Computer Linux
            Computer MacOS
            Computer Windows
            Computer Linux
        ]
        let connections = [[1]; [0; 2]; [1; 3]; [2]]
        let virus = Virus(0.8, 0.2, 0.3)
        Network(computers, connections, virus)
    let infectedComputers = network.Simulate()
    List.length infectedComputers |> should equal 4
    
[<Test>]
let ``Test simulation for graph with computer with 0 infect risk`` () =
    let infectedComputer = Computer Windows
    infectedComputer.Infected <- true;
    let network =
        let computers = [
            infectedComputer
            Computer Linux
            Computer MacOS
            Computer Windows
            Computer MacOS
        ]
        let connections = [[1; 2; 3; 4]; [0]; [0]; [0]; [0]]
        let virus = Virus(0.8, 0, 0.3)
        Network(computers, connections, virus)
    let infectedComputers = network.Simulate()
    List.length infectedComputers |> should equal 4