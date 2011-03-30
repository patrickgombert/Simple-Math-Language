// Patrick Gombert

module Project1c.Operators

type expression =
    | Const of int 
    | Plus of expression * expression 
    | Minus of expression * expression
    | Times of expression * expression
    | Div of expression * expression
    | Neg of expression
    | Var of string
    | If of expression * expression * expression
    | App of expression * expression
    | Fun of string * expression * Map<string, expression>
    | MkFun of string * expression

let rec eval (arg, m: Map<string, expression>) =
    match arg with 
    | Const(arg) -> Const(arg)
    | Plus (l, r)-> 
         match (eval(l, m), eval(r, m)) with
         | (Const(a1), Const(a2)) -> Const(a1 + a2)
         | _ -> Plus(l, r)
    | Minus (l, r)-> 
         match (eval(l, m), eval(r, m)) with
         | (Const(s1), Const(s2)) -> Const(s1 - s2)
         | _ -> Minus(l, r)
    | Times (l, r)-> 
         match (eval(l, m), eval(r, m)) with
         | (Const(m1), Const(m2)) -> Const(m1 * m2)
         | _ -> Times(l, r)
    | Div (l, r)-> 
         match (eval(l, m), eval(r, m)) with
         | (Const(a1), Const(a2)) -> Const(a1 / a2)
         | _ -> Div(l, r)
    | Neg (Const(n)) -> Const(n * -1)
    | Var arg -> m.[arg]
    | If (arg1, arg2, arg3) ->
        match (eval(arg1, m), eval(arg2, m), eval(arg3, m)) with
        | (Const(a1), Const(a2), Const(a3)) -> if a1.Equals(0) then Const(a3) else Const(a2)
        | _ -> If(arg1, arg2, arg3)
    | App (arg1, arg2) ->
        match (eval(arg1, m), eval(arg2, m)) with
        | (Fun(arg1, arg2, mm), arg3) -> eval(arg2, m.Add(arg1, arg3))
        | _ -> App(arg1, arg2)
    | Fun (x, b, m) -> Fun(x, b, m)
    | MkFun (a, b) -> Fun(a, b, Map.empty)