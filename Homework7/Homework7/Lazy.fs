module Homework7.Lazy

type ILazy<'a> =
    abstract member Get: unit -> 'a

type Lazy<'a>(supplier : unit -> 'a) =
    let mutable value = None
    interface ILazy<'a> with
        member this.Get() : 'a =
            match value with
            | Some x -> x
            | None ->
                let temp = supplier ()
                value <- Some(temp)
                temp
                
