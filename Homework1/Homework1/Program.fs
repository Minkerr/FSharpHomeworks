let factorial n = 
    let rec factorialRecursive n acc i =
        if i = n then
            acc
        else
            factorialRecursive
                n
                (acc * (n - i))
                (i + 1)   
    factorialRecursive n 1 0
            
let fibonacci n = 
    let rec fibonacciRecursive n acc1 acc2 i =
        if i = n then
            acc2
        else
            fibonacciRecursive
                n
                (acc1 + acc2)
                acc1
                (i + 1)
    fibonacciRecursive n 1 0 0 

let reverseList list = 
    let rec reverseListRecursive list res =
        if List.length list = 0 then
            res
        else
            reverseListRecursive
                (List.tail list)
                (List.head list :: res)
    reverseListRecursive list []
            
let degreeRow n m =
    let rec degreeRowRecursive n m acc num i =
        if n + m + 1 = i then
            reverseList acc
        else
            let acc = if i < n then acc else num :: acc
            degreeRowRecursive
                n
                m
                acc
                (num * 2)
                (i + 1) 
    degreeRowRecursive n m [] 1 0 
          
let searchNumberIndex list target = 
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
    searchNumberIndexRecursive list target 0
