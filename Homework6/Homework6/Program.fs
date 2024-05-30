module Homework6

type RoundingBuilder(accuracy : int) =
    member this.Bind(x, f) =
        f x
    member this.Return(x : float) =
        System.Math.Round(x, accuracy) 
    

