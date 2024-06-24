module Homework4.PointFree

let multiplyListByNumber x l = List.map (fun y -> y * x) l

let multiplyListByNumber'1 x: int list -> int list =
    List.map (fun y -> y * x)
    
let multiplyListByNumber'2 x: int list -> int list =
    List.map (fun y -> (*) x y)
    
let multiplyListByNumber'3 x: int list -> int list =
    List.map <| ((*) x)
    
let multiplyListByNumber'4 x: int list -> int list =
    List.map <| (*) x
    
let multiplyListByNumber'5 x: int list -> int list =
    (List.map << (*)) x
    
let multiplyListByNumber'6: int -> int list -> int list =
    (List.map << (*))