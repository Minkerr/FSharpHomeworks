module PrepositionCalculate
    
type BinaryOperation =
    | Add 
    | Subtract 
    | Multiply 
    | Divide
    
type Proposition =
    | Number of int
    | BinaryOperation of Proposition * BinaryOperation * Proposition

let rec calculate root =
    match root with
    | Number(x) -> x
    | BinaryOperation(l, o, r) ->
        match o with
        | Add -> calculate l + calculate r
        | Subtract -> calculate l - calculate r
        | Multiply -> calculate l * calculate r
        | Divide -> calculate l / calculate r
