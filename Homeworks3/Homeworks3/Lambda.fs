module Lambda

type variation = Name of string

type term =
    |Var of variation
    |App of term * term
    |Abs of variation * term

let rec FV lambda =
    match lambda with
    | Var(x) -> Set.ofList [Var(x)]
    | Abs(x, y) -> Set.difference (FV(y)) (Set.ofList [Var(x)])
    | App(x, y) -> Set.union (FV(x)) (FV(y)) 
    
let rec BV lambda =
    match lambda with
    | Var(x) -> Set.empty
    | Abs(x, y) -> Set.union (Set.ofList [Var(x)]) (BV(y)) 
    | App(x, y) -> Set.union (BV(x)) (BV(y))
    
let newVar s t =
    let rec newVarRec s t var =
        let varMapped = Var(Name(var))
        if (not (Set.contains varMapped (FV s))) && (not (Set.contains varMapped (FV t))) then
            Name(var)
        else
            newVarRec s t (string (char(int (char var) + 1)))
    newVarRec s t "a"

let rec substitution lambda var (t : term) =
    match lambda with
    | Var(x) when Var(x) = var ->  t
    | Var(y) when not (Var(y) = var) -> Var(y)
    | App(s1, s2) -> App(substitution s1 var t, substitution s2 var t)
    | Abs(x, s) when Var(x) = var -> Abs(x, s)
    | Abs(y, s) when (not (Set.contains (Var(y)) (FV t)) || not (Set.contains var (FV s))) ->
        Abs(y, substitution s var t)
    | Abs(y, s) -> Abs(newVar s t, substitution (substitution s (Var(y)) (Var(newVar s t))) var t)

let betaReduction lambda=
    match lambda with
    | App(Abs(x, s), t) -> substitution s (Var(x)) t
    | _ -> lambda
    