module PrepositionCalculate

type Proposition =
    | Number of int
    | Add of Proposition * Proposition
    | Subtract of Proposition * Proposition
    | Multiply of Proposition * Proposition
    | Divide of Proposition * Proposition
    
let rec calculate prep =
    match prep with
    | Number(x) -> x
    | Add(x1, x2) -> calculate x1 + calculate x2
    | Subtract(x1, x2) -> calculate x1 - calculate x2
    | Multiply(x1, x2) -> calculate x1 * calculate x2
    | Divide(x1, x2) -> calculate x1 / calculate x2

