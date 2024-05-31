module Homework6

/// <summary>
/// Monad for rounding calculation results with float 
/// </summary>
type RoundingBuilder(accuracy : int) =
    member this.Bind(x : float, f) =
        f (System.Math.Round(x, accuracy))
    member this.Return(x : float) =
        System.Math.Round(x, accuracy)
         
exception IncorrectInputString of string

/// <summary>
/// Monad for making calculation with numbers from string
/// </summary>
type StringCalculateBuilder() =
    member this.Bind(x : string, f) =
        match (System.Double.TryParse x) with
        | true, double -> f double
        | _ -> None
    member this.Return(x) =
        Some x
        
