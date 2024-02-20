let rec factorial n acc i =
    if i = n then
        acc
    else
        factorial
            n
            (acc * (n - i))
            (i + 1)      

let rec fibonacci n acc1 acc2 i =
    if i = n then
        acc2
    else
        fibonacci
            n
            (acc1 + acc2)
            acc1
            (i + 1)

let rec reverseList list res =
    if List.length list = 0 then
        res
    else
        reverseList
            (List.tail list)
            (List.head list :: res)
            
let rec degreeRow n m acc num i =
    if n + m + 1 = i then
        reverseList acc []
    else
        let acc = if i < n then acc else num :: acc
        degreeRow
            n
            m
            acc
            (num * 2)
            (i + 1)
                
let rec searchNumberIndex list target i =
    if List.length list = 0 then
        -1
    else if List.head list = target then
        i
    else
        searchNumberIndex
            (List.tail list)
            target
            (i+1)
            