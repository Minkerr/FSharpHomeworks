module Homework4.CorrectBracketSequence

let checkStringForCorrectBracketSequence input =
    let rec CBSRec (input : string) i (stack : char List) =
        if i = input.Length then stack.IsEmpty else 
            match input[i] with
            | c when (c = '(' || c = '{' || c = '[') -> CBSRec input (i + 1) (c :: stack)
            | ')' -> if stack.IsEmpty || not (stack.Head = '(') then false else CBSRec input (i + 1) stack.Tail
            | '}' -> if stack.IsEmpty || not (stack.Head = '{') then false else CBSRec input (i + 1) stack.Tail
            | ']' -> if stack.IsEmpty || not (stack.Head = '[') then false else CBSRec input (i + 1) stack.Tail
            | _ -> CBSRec input (i + 1) stack
    CBSRec input 0 []
            
let checkStringForCorrectBracketSequenceWithLessCopyPaste input =
    let getOpenBracket c =
        match c with
        |')' -> '('
        |'}' -> '{'
        |']' -> '['
    let rec CBSRec (input : string) i (stack : char List) =
        if i = input.Length then stack.IsEmpty else 
            match input[i] with
            | c when (c = '(' || c = '{' || c = '[') -> CBSRec input (i + 1) (c :: stack)
            | c when (c = ')' || c = '}' || c = ']') -> 
                if stack.IsEmpty || not (stack.Head = getOpenBracket c) then false else CBSRec input (i + 1) stack.Tail
            | _ -> CBSRec input (i + 1) stack
    CBSRec input 0 []