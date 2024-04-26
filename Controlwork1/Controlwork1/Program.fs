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

printf "%d" 
  
