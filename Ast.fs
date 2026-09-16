namespace Solala

module Type =
    type TypeVar = { label: string; mutable parent: t }

    and t =
        | Bool
        | Int
        | String
        | Date
        | List of t
        | Var of TypeVar

    exception RuntimeTypeError of string

    let genTypeVar =
        let mutable i = 0L

        fun () ->
            let j = i
            i <- i + 1L

            let rec tvar =
                Var
                    { label = $"typevar-{j}"
                      parent = tvar }

            tvar

    let union t1 t2 =
        let find t =
            let mutable depth = 0

            let rec find =
                function
                | Var tvar ->
                    let t = find tvar.parent
                    depth <- depth + 1
                    tvar.parent <- t
                    t
                | t -> t in

            find t, depth in

        match t1, t2 with
        | t1, t2 when t1 = t2 -> t1
        | Var tvar1, Var tvar2 ->
            let p1, d1 = find tvar1.parent
            let p2, d2 = find tvar2.parent

            if d1 < d2 then
                tvar2.parent <- p1
                p1
            else
                tvar1.parent <- p2
                p2
        | Var tvar, t
        | t, Var tvar ->
            tvar.parent <- t
            t
        | _ -> failwith "Type mismatch!"

    let rec concreteType =
        function
        | Var { parent = t } as tvar ->
            if obj.Equals(t, tvar) then
                failwith "Could not infer concrete type"

            concreteType t
        | t -> t

module Value =
    type t =
        | Bool of bool
        | Int of int
        | String of string
        | Date of System.DateOnly
        | List of t list

    let _true = Bool true
    let _false = Bool false

    let b b = if b then _true else _false

    let isTrue =
        function
        | Bool false
        | List [] -> false
        | _ -> true // False and empty list are false values, everything else is true

    let isFalse = isTrue >> not

    let band a b =
        if isTrue a && isTrue b then _true else _false

    let bor a b =
        if isFalse a && isFalse b then _false else _true

    let bnot a = if isTrue a then _false else _true

module Ast =
    (**
    define symbol child Barnet. -- Already defined, do not query, its value is "true".
    define symbol applicant Ansøger.

    define assertion is_guardian guardian of child equals applicant. -- Comparison of properties, query for nested property.
    define assertion permanent_handicap is_permanent of handicap of child. -- Does this property exist? Query!

    define benefit some_name
      provides Kompensation af kørselsudgifter
      requires
        is_guardian and permanent_handicap.


*)
    type Symbol = { name: string; description: string }

    type 't Ref = { typ: 't; path: string list } // Infer types!

    type Unary = Not

    type Binary =
        | And
        | Or
        | Equals
        | LessThan
        | In // x > y => y < x

    type 't Expression =
        | Const of Value.t
        | Ref of 't Ref
        | ApplyUnary of Unary * 't Expression
        | ApplyBinary of Binary * 't Expression * 't Expression

    type Judgment = { description: string }

    type 't Assertion =
        { assertion: 't Expression
          description: string }

    type 't Condition =
        | Judgment of Judgment
        | Assertion of 't Assertion

    type 't Benefit =
        { description: string
          provides: string
          requires: 't Condition list }

// let typeRef types = function
//     | Ref { typ = typ; path = path; }

module Continuation =
    type t<'a, 'b> =
        | Value of 'b
        | Query of string list * Type.t * ('a -> t<'a, 'b>)

    let value x = Value x

    let query<'a> typ path =
        Query(path, typ, fun (x: 'a) -> Value x)

    let rec bind (f: 'd -> t<'a, 'b>) : t<'a, 'd> -> t<'a, 'b> =
        function
        | Value x -> f x
        | Query(s, typ, k) -> Query(s, typ, fun x -> bind f (k x))

    let inline (>>=) m f = bind f m

    type ContinuationBuilder private () =
        member _.Bind(m, f) = bind f m
        member _.Return(x: 'a) : t<_, 'a> = Value x
        member _.ReturnFrom x = x
        member _.Zero() = Value Value._false
        static member val Instance = ContinuationBuilder()

    let cont = ContinuationBuilder.Instance

module Eval =
    open Ast

    let cont = Continuation.cont
    let query = Continuation.query
    let (>>=) = Continuation.(>>=)

    let evalRef { typ = typ; path = path } : Continuation.t<Value.t, Value.t> = query (Type.concreteType typ) path

    let rec evalExpr: _ Expression -> _ =
        function
        | Ref reference -> evalRef reference

        | Const value -> cont { return value }

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
                    if Value.isTrue b then return! evalExpr e2

                | Or ->
                    match! evalExpr e1 with
                    | Value.Bool false -> return! evalExpr e2
                    | v -> return v

                | LessThan ->
                    let! v1 = evalExpr e1
                    let! v2 = evalExpr e2

                    match v1, v2 with
                    | Value.Date d1, Value.Date d2 -> return Value.b (d1 < d2)
                    | Value.Int i1, Value.Int i2 -> return Value.b (i1 < i2)
                    | _ -> ()

                | In ->
                    let! v = evalExpr e1
                    match! evalExpr e2 with
                    | Value.List vs -> return Value.b (List.contains v vs)
                    | _ -> ()
            }

    type Result =
        | Success
        | Failure of string
        | Discretional of string

    let evalCondition =
        function
        | Assertion { description = d; assertion = a } ->
            cont {
                let! b = evalExpr a
                return if Value.isTrue b then Success else Failure d
            }
        | Judgment { description = description } -> cont { return Discretional description }

    type EligibilityConditions =
        { discretionals: string list
          failures: string list }

    let evalBenefit { requires = rqs } : Continuation.t<EligibilityConditions, _> =
        let conditions = cont { return { discretionals = []; failures = [] } }

        let cons conds cond =
            cont {
                let! (conds: EligibilityConditions) = conds

                match! evalCondition cond with
                | Success -> return conds
                | Failure descr ->
                    return
                        { conds with
                            failures = descr :: conds.failures }
                | Discretional descr ->
                    return
                        { conds with
                            discretionals = descr :: conds.discretionals }
            }

        List.fold cons conditions rqs
