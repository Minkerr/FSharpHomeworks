module Lambda

type Term =
    | Var of string
    | App of Term * Term
    | Abs of string * Term

let rec fv lambda =
    match lambda with
    | Var(x) -> Set.ofList [Var(x)]
    | App(x, y) -> Set.union (fv(x)) (fv(y)) 
    | Abs(x, y) -> Set.difference (fv(y)) (Set.ofList [Var(x)])
    
let rec bv lambda =
    match lambda with
    | Var(x) -> Set.empty
    | Abs(x, y) -> Set.union (Set.ofList [Var(x)]) (bv(y)) 
    | App(x, y) -> Set.union (bv(x)) (bv(y))
    
let newVar s t =
    let rec newVarRec s t var =
        let varMapped = Var(var)
        if (not (Set.contains varMapped (fv s))) && (not (Set.contains varMapped (fv t))) then
            var
        else
            newVarRec s t (string (char(int (char var) + 1)))
    newVarRec s t "a"

let rec substitution lambda var (t : Term) =
    match lambda with
    | Var(x) when Var(x) = var ->  t
    | Var(y) when not (Var(y) = var) -> Var(y)
    | App(s1, s2) -> App(substitution s1 var t, substitution s2 var t)
    | Abs(x, s) when Var(x) = var -> Abs(x, s)
    | Abs(y, s) when (not (Set.contains (Var(y)) (fv t)) || not (Set.contains var (fv s))) ->
        Abs(y, substitution s var t)
    | Abs(y, s) -> Abs(newVar s t, substitution (substitution s (Var(y)) (Var(newVar s t))) var t)

let rec betaReductionStep lambda=
    match lambda with
    //| App(Abs(x, s), App(t1, t2)) -> App(substitution s (Var(x)) t1, t2)
    | App(Abs(x, s), t) -> substitution s (Var(x)) t
    //| App(x, t) -> App(betaReductionStep x, t)
    //| App(t, Abs(x, s)) -> substitution s (Var(x)) t
    | _ -> lambda
    
let rec betaReduce lambda =
    let next = betaReductionStep lambda
    if lambda = next then
        lambda
    else
        betaReduce next
        
