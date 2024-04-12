module Simulator

open System

type OS = Windows | Linux | MacOS

type Virus(windowsRisk, linuxRisk, macosRisk) =
    member this.defineRisk(os: OS) =
        match os with
        | Windows -> windowsRisk
        | Linux -> linuxRisk
        | MacOS -> macosRisk

type Computer(os: OS) =
    let mutable infected = false
    member this.OS = os
    member this.Infected
        with get() = infected
        and set(value) = infected <- value
    member this.TryInfect(risk : float) =
        let r = Random()
        let result = (r.NextDouble() < risk)
        infected <- result
        result

type Network(computers: Computer list, connections: int list list, virus: Virus) =
    member this.Computers = computers
    member this.Connections = connections
    member private this.getInfectedComputers = (computers
                        |> List.mapi (fun i computer -> if computer.Infected then Some i else None)
                        |> List.choose id)
    member this.Simulate() =
        let rec simulateStep infectedComputers =
            printfn "Infected computers: %A" (List.map (fun i -> i + 1) infectedComputers)
            let connectedComputers =
                infectedComputers
                |> List.collect (fun i ->
                    connections[i]
                    |> List.filter (fun i -> not (List.contains i infectedComputers))
                    |> List.filter (fun i -> not computers[i].Infected)
                )
            let newInfectedComputers =
                connectedComputers
                |> List.map (fun i -> (i, computers[i]))
                |> List.filter (fun (i, c) -> c.TryInfect(virus.defineRisk(c.OS)))
                |> List.map fst
            if List.isEmpty connectedComputers then
                infectedComputers
            else
                simulateStep (infectedComputers @ newInfectedComputers)
        simulateStep this.getInfectedComputers

