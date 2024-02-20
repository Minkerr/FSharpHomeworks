let rec factorial n acc i =
    if i = n then
        acc
    else
        factorial
            (n)
            (acc * (n - i))
            (i + 1)

//printfn "%d" (factorial 0 1 0)
