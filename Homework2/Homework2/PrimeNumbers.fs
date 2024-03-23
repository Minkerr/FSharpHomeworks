module PrimeNumbers

let primeNumbers = 
    2 |> Seq.unfold (fun index ->
        let rec nextPrime n =
            let rec isPrime d n =
                if d = 1 then
                    true
                else if n % d = 0 then
                    false 
                else
                    isPrime (d - 1) n
            if isPrime (n / 2) n then
                n
            else
                nextPrime (n + 1)
        Some(index, nextPrime (index + 1)))

