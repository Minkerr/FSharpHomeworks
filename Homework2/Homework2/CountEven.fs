module CountEven

let countEvenFilter list =
    list |> Seq.filter (fun x -> x % 2 = 0) |> Seq.length 

let countEvenFold list =
    list |> Seq.fold (fun acc x -> acc + ((abs x + 1) % 2)) 0
    
let countEvenMap list =
    let number = list |> Seq.map (fun x -> abs x % 2) |> Seq.sort |> Seq.tryFindIndex (fun x -> x = 1)
    match number with
    | Some(number) -> number
    | _ -> Seq.length list
