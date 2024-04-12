module Program

open Simulator

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
    let connections = [[1; 2; 3; 4]; [0]; [0]; [0]; [0]]
    let virus = Virus(0.8, 0.2, 0.3)
    Network(computers, connections, virus)

let infectedComputers = network.Simulate()

