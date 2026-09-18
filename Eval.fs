namespace Solala

module Eval =
    open Ast

    let cont = Continuation.cont
    let query = Continuation.query
    let (>>=) = Continuation.(>>=)

    let evalRef { typ = typ; path = path } : Continuation.t<Value.t, Value.t> = query typ path

    let rec evalExpr: _ Expression -> _ =
        function
        | Ref reference -> evalRef reference

        | Const value -> cont { return value }

        | List es ->
            cont {
                let! vs =
                    List.foldBack
                        (fun e vs ->
                            cont {
                                let! v = evalExpr e
                                let! vs = vs
                                return v :: vs
                            })
                        es
                        (Continuation.value []: Continuation.t<_, Value.t list>)

                return Value.List vs
            }

        | ApplyUnary(Not, e) ->
            cont {
                let! b = evalExpr e
                return Value.bnot b
            }

        | ApplyBinary(op, e1, e2) ->
            cont {
                match op with
                | Equals ->
                    let! v1 = evalExpr e1
                    let! v2 = evalExpr e2
                    return Value.b (v1 = v2)

                | And ->
                    let! b = evalExpr e1

                    if Value.isTrue b then
                        return! evalExpr e2

                | Or ->
                    match! evalExpr e1 with
                    | Value.Bool false -> return! evalExpr e2
                    | v -> return v

                | LessThan ->
                    let! v1 = evalExpr e1
                    let! v2 = evalExpr e2

                    match v1, v2 with
                    | Value.Int i1, Value.Int i2 -> return Value.b (i1 < i2)
                    | _ -> ()

                | Before ->
                    let! v1 = evalExpr e1
                    let! v2 = evalExpr e2

                    match v1, v2 with
                    | Value.Date d1, Value.Date d2 -> return Value.b (d1 < d2)
                    | _ -> ()

                | In ->
                    let! v = evalExpr e1

                    match! evalExpr e2 with
                    | Value.List vs -> return Value.b (List.contains v vs)
                    | _ -> ()
            }

    type Criterion =
        | Met
        | Unmet of string
        | Discretional of string

    let evalCondition =
        function
        | Assertion { description = d; expression = e } ->
            cont {
                let! b = evalExpr e
                return if Value.isTrue b then Met else Unmet d
            }
        | Judgment { description = description } -> cont { return Discretional description }

    type EligibilityConditions =
        { discretionals: string list
          failures: string list }

    let evalBenefit { requires = rqs } =
        let conditions = cont { return { discretionals = []; failures = [] } }

        let cons conds cond =
            cont {
                let! (conds: EligibilityConditions) = conds

                match! evalCondition cond with
                | Met -> return conds
                | Unmet descr ->
                    return
                        { conds with
                            failures = descr :: conds.failures }
                | Discretional descr ->
                    return
                        { conds with
                            discretionals = descr :: conds.discretionals }
            }

        List.fold cons conditions rqs
