let rec factorialRecursive n acc i =
    if i = n then
        acc
    else
        factorialRecursive
            n
            (acc * (n - i))
            (i + 1)      

let factorial n = factorialRecursive n 1 0

let rec fibonacciRecursive n acc1 acc2 i =
    if i = n then
        acc2
    else
        fibonacciRecursive
            n
            (acc1 + acc2)
            acc1
            (i + 1)
            
let fibonacci n = fibonacciRecursive n 1 0 0 

let rec reverseListRecursive list res =
    if List.length list = 0 then
        res
    else
        reverseListRecursive
            (List.tail list)
            (List.head list :: res)
            
let reverseList list = reverseListRecursive list []
            
let rec degreeRowRecursive n m acc num i =
    if n + m + 1 = i then
        reverseListRecursive acc []
    else
        let acc = if i < n then acc else num :: acc
        degreeRowRecursive
            n
            m
            acc
            (num * 2)
            (i + 1)
            
let degreeRow n m = degreeRowRecursive n m [] 1 0 
                
let rec searchNumberIndexRecursive list target i =
    if List.length list = 0 then
        None
    else if List.head list = target then
        Some(i)
    else
        searchNumberIndexRecursive
            (List.tail list)
            target
            (i + 1)
          
let searchNumberIndex list target = searchNumberIndexRecursive list target 0
