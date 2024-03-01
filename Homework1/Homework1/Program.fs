let factorial n =
    if n < 0 then None else
        let rec factorialRecursive n acc i =
            if i = n then acc else
                
            factorialRecursive n (acc * (n - i)) (i + 1)   
        Some(factorialRecursive n 1 0)
            
let fibonacci n =
    if n < 0 then None else
        let rec fibonacciRecursive n acc1 acc2 i =
            if i = n then acc2 else
                
            fibonacciRecursive n (acc1 + acc2) acc1 (i + 1)
        Some(fibonacciRecursive n 1 0 0) 

let reverse list = 
    let rec reverseListRecursive list res =
        match list with
         | [] -> res
         | head :: tail -> reverseListRecursive tail (head :: res)
    reverseListRecursive list []
            
let degreeRow n m =
    let rec degreeRowRecursive n m acc num i =
        if m + 1 = i then acc else
            
        degreeRowRecursive n m (num :: acc) (num / 2) (i + 1) 
    degreeRowRecursive n m [] (int (2.0 ** float (m + n))) 0 
          
let tryFind list target = 
    let rec tryFindRecursive list target i =
        match list with
         | [] -> None
         | head :: tail when head = target -> Some(i)
         | _ -> tryFindRecursive (List.tail list) target (i + 1)
    tryFindRecursive list target 0
