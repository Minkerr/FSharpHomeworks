module Homework6

type RoundingBuilder(accuracy : int) =
    member this.Bind(x, f) =
        f x
    member this.Return(x : float) =
        System.Math.Round(x, accuracy) 
    
exception IncorrectInputString of string
    
type StringCalculateBuilder() =
    let TryParseNumber (str : string) =
        match System.Double.TryParse str with
        | true, double -> double
        | _ -> raise (IncorrectInputString("String should be a number"))
    member this.Bind(x, f) =
        f (TryParseNumber x)
    member this.Return(x) = x
        
type MaybeBuilder() =
    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a
    member this.Return(x) = 
        Some x
   
let divide x y =
    match y with
    | 0.0 -> None
    | _ -> Some (x / y)   

let maybe = MaybeBuilder()

let resistance r1 r2 r3 = 
    maybe {
        let! r1' = divide 1.0 r1
        let! r2' = divide 1.0 r2
        let! r3' = divide 1.0 r3
        let! r = divide 1.0 (r1' + r2' + r3')
        return r
    }