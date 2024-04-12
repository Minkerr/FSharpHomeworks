module Simulator

open System

type OS = Windows | Linux | MacOS

type Computer(os: OS) =
    let mutable infected = false
    member this.OS = os
    member this.Infected
        with get() = infected
        and set(value) = infected <- value
    member this.TryInfect() =
        let r = Random()
        match this.OS with
        | Windows -> infected <- (r.NextDouble() < 0.8)
        | Linux -> infected <- (r.NextDouble() < 0.2)
        | MacOS -> infected <- (r.NextDouble() < 0.1)

type Network(computers: Computer list, connections: (int * int) list) =
    member this.Computers = computers
    member this.Connections = connections
    member private this.getInfectedComputers = (computers
                        |> List.mapi (fun i computer -> if computer.Infected then Some i else None)
                        |> List.choose id)
    member this.Simulate() =
        let rec simulateStep infectedComputers =
            printfn "Infected computers: %A" (List.map (fun i -> i + 1) infectedComputers)
            let newInfectedComputers =
                infectedComputers
                |> List.collect (fun i ->
                    connections
                    |> List.filter (fun (a, b) -> a = i || b = i)
                    |> List.map (fun (a, b) -> if a = i then b else a)
                    |> List.filter (fun i -> not (List.contains i infectedComputers))
                    |> List.filter (fun i -> not computers[i].Infected)
                )
            if List.isEmpty newInfectedComputers then
                infectedComputers
            else
                simulateStep (infectedComputers @ newInfectedComputers)
        simulateStep this.getInfectedComputers

