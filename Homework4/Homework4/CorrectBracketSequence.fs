module Homework4.CorrectBracketSequence

let checkStringForCorrectBracketSequence input =
    let closingBrackets = [')'; '}'; ']']
    let openingBrackets = ['('; '{'; '[']
    let getOpeningBracket c =
        match c with
        |')' -> '('
        |'}' -> '{'
        |']' -> '['
    let rec CBSRec (input : string) i (stack : char List) =
        if i = input.Length then stack.IsEmpty else 
            match input[i] with
            | c when (List.contains c openingBrackets) -> CBSRec input (i + 1) (c :: stack)
            | c when (List.contains c closingBrackets) ->
                match stack with
                | [] -> false
                | head :: _ when head <> getOpeningBracket c -> false
                | _ -> CBSRec input (i + 1) stack.Tail
            | _ -> CBSRec input (i + 1) stack
    CBSRec input 0 []