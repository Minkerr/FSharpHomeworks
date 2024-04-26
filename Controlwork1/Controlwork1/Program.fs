module Controlwork

let square n =
    let constructLine x =
        if (x = n || x = 1) then (String.replicate n "*")
        else ("*" + (String.replicate (n - 2) " ") + "*")
    String.concat "\n" (List.map constructLine [1..n])
    
let sumEvenFibonacciLessThanMillion () =
    let fibonacci n =
        let rec fibonacciRecursive n acc1 acc2 i =
            if i = n then acc2 else
                fibonacciRecursive n (acc1 + acc2) acc1 (i + 1)
        fibonacciRecursive n 1 0 0
    let rec sumEvenFibonacciRec sum i =
        let fibonacciToAdd = if (fibonacci i) % 2 = 0 then (fibonacci i) else 0
        if fibonacciToAdd > 1000000 then sum
        else sumEvenFibonacciRec (sum + fibonacciToAdd) (i + 1)
    sumEvenFibonacciRec 0 1

type PriorityQueue() =
    let mutable queue = []

    member this.Enqueue(item, priority) =
        let rec enqueueRec items newItem =
            match items with
            | [] -> [newItem]
            | (_, localPriority) :: _ when priority <= localPriority -> (item, priority) :: items
            | head :: tail -> head :: (enqueueRec tail newItem)
        queue <- enqueueRec queue (item, priority)

    member this.Dequeue() =
        match queue with
        | [] -> failwith "Queue is empty"
        | head :: tail ->
            queue <- tail
            fst head

    member this.Peek() =
        match queue with
        | [] -> failwith "Queue is empty"
        | head :: _ -> fst head

