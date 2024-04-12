module Simulator

open System

type OS = Windows | Linux | MacOS

/// <summary>
/// Contains information about infect risk for every OS 
/// </summary>
type Virus(windowsRisk, linuxRisk, macosRisk) = 
    member this.defineRisk(os: OS) =
        match os with
        | Windows -> windowsRisk
        | Linux -> linuxRisk
        | MacOS -> macosRisk

/// <summary>
/// One element of Network. Has one OS and can be infected 
/// </summary>
type Computer(os: OS) =
    let mutable infected = false
    member this.OS = os
    member this.Infected
        with get() = infected
        and set(value) = infected <- value
    /// <summary>
    /// Computer becomes infected with a given chance. Returns true if it became
    /// </summary>
    member this.TryInfect(chance : float) =
        let result = (Random().NextDouble() < chance)
        infected <- result
        result

/// <summary>
/// Simulation of computer network with virus. Connection is set by an adjacency list
/// </summary>
type Network(computers: Computer list, connections: int list list, virus: Virus) =
    member this.Computers = computers
    member this.Connections = connections
    member private this.getInfectedComputers = (computers
            |> List.mapi (fun i computer -> if computer.Infected then Some i else None)
            |> List.choose id)
    /// <summary>
    /// Launch process of the spread of the virus 
    /// </summary>
    member this.Simulate() =
        let rec simulateStep infectedComputers =
            printfn "Infected computers: %A" (List.map (fun i -> i + 1) infectedComputers)
            let connectedNotInfectedComputers =
                infectedComputers
                |> List.collect (fun i ->
                    connections[i]
                    |> List.filter (fun i -> not computers[i].Infected)
                )
            let newInfectedComputers =
                connectedNotInfectedComputers
                |> List.filter (fun i ->
                    computers[i].TryInfect(virus.defineRisk(computers[i].OS)))
            let canInfectionContinue =
                connectedNotInfectedComputers
                |> List.map (fun i -> virus.defineRisk(computers[i].OS) > 0.001)
                |> List.contains true
            // Simulation continues if there are computer with infect chance > 0 
            if not canInfectionContinue then
                infectedComputers
            else
                simulateStep (infectedComputers @ newInfectedComputers)
        simulateStep this.getInfectedComputers

