module PrepositionCalculate

type Proposition =
    | Number of int
    | BinaryOperation of Proposition * string * Proposition
    
let rec calculate prep =
    match prep with
    | Number(x) -> x
    | BinaryOperation(l, o, r) ->
        match o with
        | "+" -> calculate l + calculate r
        | "-" -> calculate l - calculate r
        | "*" -> calculate l * calculate r
        | "/" -> calculate l / calculate r
        | _ -> failwith "Incorrect operation"

