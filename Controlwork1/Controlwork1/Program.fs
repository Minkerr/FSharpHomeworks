module Controlwork

let square n =
    let constructLine x =
        if (x = n || x = 1) then (String.replicate n "*")
        else ("*" + (String.replicate (n - 2) " ") + "*")
    String.concat "\n" (List.map constructLine [1..n])
    
let sumEvenFibonacciLessThanMillion () =
    let rec sumEvenFibonacciRecursive sum acc1 acc2 i =
        let numberToAdd = if acc2 % 2 = 0 then acc2 else 0
        if numberToAdd > 1000000 then sum
        else sumEvenFibonacciRecursive (sum + numberToAdd) (acc1 + acc2) acc1 (i + 1)
    sumEvenFibonacciRecursive 0 1 0 0

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

