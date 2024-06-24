module Homework7.Lazy

open System.Threading

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
                
type ConcurrentLazy<'a>(supplier : unit -> 'a) =
    let locker = obj()
    let mutable value = None
    interface ILazy<'a> with
        member this.Get() : 'a =
            match value with
            | Some x -> x
            | None ->
                lock locker ( fun () ->
                    let temp = supplier ()
                    value <- Some(temp)
                    temp) 
                
type LockFreeLazy<'a>(supplier : unit -> 'a) =
    let mutable value = None
    interface ILazy<'a> with
        member this.Get() : 'a =
            match value with
            | Some x -> x
            | None ->
                let temp = supplier ()
                Volatile.Write(ref value, Some(temp)) 
                temp