let rec factorial n acc i =
    if i = n then
        acc
    else
        factorial
            (n)
            (acc * (n - i))
            (i + 1)
            
//printfn "%d" (factorial 0 1 0)        

let rec fibonacci n acc1 acc2 i =
    if i = n then
        acc2
    else
        fibonacci
            (n)
            (acc1 + acc2)
            (acc1)
            (i + 1)

//printfn "%d" (fibonacci 11 1 0 0)
