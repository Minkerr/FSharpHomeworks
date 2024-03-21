module Lambda.Tests

open NUnit.Framework
open Lambda
open FsUnit

[<Test>]
let ``FV find all free variables in lambda term`` () =
    let x = Name("x")
    let y = Name("y")
    let z = Name("z")
    let lambda = App(Abs(x, Abs(y, Var(z))), Abs(x, App(Var(z), Var(x)))) //(λx y.x)(λx.z x)
    FV lambda |> should contain (Var(z))
    Set.count (FV lambda) |> should equal 1

[<Test>]
let ``BV find all bonded variables in lambda term`` () =
    let x = Name("x")
    let y = Name("y")
    let z = Name("z")
    let lambda = App(Abs(x, Abs(y, Var(z))), Abs(x, App(Var(z), Var(x)))) //(λx y.x)(λx.z x)
    BV lambda |> should contain (Var(x))
    BV lambda |> should contain (Var(y))
    Set.count (BV lambda) |> should equal 2
    
[<Test>]
let ``newVar should find new name for bonded variable`` () =
    let a = Name("a")
    let b = Name("b")
    let c = Name("c")
    let lambda1 = Abs(c, Abs(c, Var(a))) //(λc c.a)
    let lambda2 = Abs(c, App(Var(a), Var(b))) //(λc.a b)
    newVar lambda1 lambda2 |> should equal (Name("c"))
    
[<Test>]
let ``Substraction should work for variables`` () =
    let x = Name("x")
    let y = Name("y")
    let z = Name("z")
    let term = Abs(z, Var(z))
    substitution (Var(x)) (Var(x)) term |> should equal term
    substitution (Var(y)) (Var(x)) term |> should equal (Var(y))
    
[<Test>]
let ``Substitution should work for application`` () =
    let x = Name("x")
    let z = Name("z")
    let lambda1 = App(Var(x), Var(x))
    let lambda2 = Abs(z, Var(z))
    substitution lambda1 (Var(x)) lambda2 |> should equal (App(lambda2, lambda2))
    
[<Test>]
let ``Substitution should work for abstraction in place of bonded variable`` () =
    let x = Name("x")
    let y = Name("y")
    let z = Name("z")
    let term = Abs(x, Var(y))
    substitution term (Var(x)) (Var(z)) |> should equal term
    
[<Test>]
let ``Substitution should work for abstraction without name conflict`` () =
    let x = Name("x")
    let y = Name("y")
    let z = Name("z")
    let term1 = Abs(y, Var(x))
    let term2 = Abs(z, Var(z))
    substitution term1 (Var(x)) term2 |> should equal (Abs(y, Abs(z, Var(z))))
    
[<Test>]
let ``Substitution should work for abstraction with name conflict`` () =
    let a = Name("a")
    let x = Name("x")
    let y = Name("y")
    let term1 = Abs(y, App(Var(x), Var(y)))
    substitution term1 (Var(x)) (Var(y)) |> should equal (Abs(a, App(Var(y), Var(a))))
    
[<Test>]
let ``Beta reduction should calculate with normal strategy`` () =
    let x = Name("x")
    let y = Name("y")
    let term1 = Abs(x, Var(y))
    let term2 = Abs(x, App(Var(x), App(Var(x), Var(x))))
    betaReduction (App(term1, App(term2, term2))) |> should equal (Var(y))