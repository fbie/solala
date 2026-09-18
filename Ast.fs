namespace Solala

module Empty =
    type t = Impossible of t

module Type =
    type 't TypeVar = { label: string; mutable parent: 't }

    and 'var t =
        | Bool
        | Int
        | String
        | Date
        | List of 'var t
        | Var of 'var * 'var t TypeVar

    exception RuntimeTypeError of string

    type vtype = unit t
    type ctype = Empty.t t

    let genTypeVar =
        let mutable i = 0L

        fun () ->
            let j = i
            i <- i + 1L

            let rec tvar = // Is its own parent.
                Var(
                    (),
                    { label = $"typevar-{j}"
                      parent = tvar }
                )

            tvar

    let find t =
        let mutable depth = 0

        let rec find =
            function
            | Var(_, tvar) ->
                let t = find tvar.parent
                depth <- depth + 1
                tvar.parent <- t
                t
            | t -> t

        find t, depth

    let union t1 t2 =
        match t1, t2 with
        | t1, t2 when t1 = t2 -> t1
        | Var(_, tvar1), Var(_, tvar2) ->
            let p1, d1 = find tvar1.parent
            let p2, d2 = find tvar2.parent

            if d1 < d2 then
                tvar2.parent <- p1
                p1
            else
                tvar1.parent <- p2
                p2
        | Var(_, tvar), t
        | t, Var(_, tvar) ->
            tvar.parent <- t
            t
        | _ -> failwith "Type mismatch!"

    let rec concreteType: vtype -> ctype =
        function
        | Var(_, { parent = t }) as tvar ->
            if obj.Equals(t, tvar) then
                failwith "Could not infer concrete type"

            let t, _ = find t
            concreteType t
        | Bool -> Bool
        | Int -> Int
        | String -> String
        | Date -> Date
        | List t -> List(concreteType t)

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
        is_guardian and permanent_handicap; // ; composes,
        age of child < 18. // . terminates.


*)
    type Symbol = { name: string; description: string }

    type 't Ref = { typ: 't; path: string list } // Infer types!

    type Unary = Not

    type Binary =
        | And
        | Or
        | Equals
        | LessThan // x > y => y < x
        | Before // x after y => y before x
        | In

    type 't Expression =
        | Const of Value.t
        | List of 't Expression list
        | Ref of 't Ref
        | ApplyUnary of Unary * 't Expression
        | ApplyBinary of Binary * 't Expression * 't Expression

    type Judgment = { description: string }

    type 't Assertion =
        { expression: 't Expression
          description: string }

    type 't Condition =
        | Judgment of Judgment
        | Assertion of 't Assertion

    type 't Benefit =
        { description: string
          provides: string
          requires: 't Condition list }

    let rec map f =
        function
        | Const v -> Const v
        | Ref reference -> Ref(f reference)
        | List es -> List(List.map (map f) es)
        | ApplyUnary(op, e) -> ApplyUnary(op, map f e)
        | ApplyBinary(op, e1, e2) -> ApplyBinary(op, map f e1, map f e2)

    let rec typeExpression typ =
        function
        | Const v -> Const v
        | List es -> List(List.map (typeExpression (Type.genTypeVar ())) es)
        | Ref reference ->
            Ref
                { reference with
                    typ = Type.union reference.typ typ }
        | ApplyUnary(Not, e) -> ApplyUnary(Not, typeExpression Type.Bool e)
        | ApplyBinary(op, e1, e2) ->
            let t t =
                ApplyBinary(op, typeExpression t e1, typeExpression t e2)

            match op with
            | Equals -> t (Type.genTypeVar ()) // Ad-hoc
            | And
            | Or -> t Type.Bool
            | LessThan -> t Type.Int
            | Before -> t Type.Date
            | In ->
                let tvar = Type.genTypeVar ()
                ApplyBinary(In, typeExpression tvar e1, typeExpression (Type.List tvar) e2)


    let rec typeBenefit (benefit: Type.vtype Benefit) : Type.ctype Benefit =
        let typeCondition: Type.vtype Condition -> Type.ctype Condition =
            function
            | Judgment { description = d } -> Judgment { description = d } // ?
            | Assertion assertion ->
                let e =
                    assertion.expression
                    |> typeExpression (Type.genTypeVar ())
                    |> map (fun r ->
                        { path = r.path
                          typ = Type.concreteType r.typ })

                Assertion
                    { description = assertion.description
                      expression = e }

        { description = benefit.description
          provides = benefit.provides
          requires = List.map typeCondition benefit.requires }

module Continuation =
    type t<'a, 'b> =
        | Value of 'b
        | Query of string list * Type.ctype * ('a -> t<'a, 'b>)

    let value<'a, 'b> (x: 'b) : t<'a, 'b> = Value x

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
        member _.ReturnFrom(x: t<_, _>) : t<_, _> = x
        member _.Zero() = Value Value._false
        static member val Instance = ContinuationBuilder()

    let cont = ContinuationBuilder.Instance

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

module Tui =
    open System
    open Ast

    type env = Map<string list, Value.t>

    let run (env: env) (benefit: Type.ctype Ast.Benefit) =
        let parse f (s: string) =
            if String.IsNullOrEmpty s then
                Ok None
            else
                try
                    Ok(Some(f s))
                with :? FormatException as e ->
                    Error e.Message

        let rec retry f x =
            match f x with
            | Error(e: string) ->
                printf $"Error: {e}"
                retry f x
            | Ok x -> x

        let query (path: string list) (t: string) () =
            let label = String.concat "of" path
            printf $"Please enter {t} for {label} and press enter:"
            Console.ReadLine()

        let rec ask path typ =
            let query label k = query path label >> parse k

            let query =
                match typ with
                | Type.Bool -> query "true or false" (bool.Parse >> Value.b)
                | Type.Int -> query "a number" (int >> Value.Int)
                | Type.Date -> query "a date" (DateOnly.Parse >> Value.Date)
                | Type.String -> query "text" Value.String
                | Type.List t ->
                    let rec loop vs () =
                        match ask path t with
                        | None -> List.rev vs
                        | Some v -> loop (v :: vs) ()

                    loop [] >> Value.List >> Option.Some >> Ok
                | _ -> failwith "Cannot happen"

            retry query () |> Option.get // Might raise.

        let lookup (env: env) path typ =
            match Map.tryFind path env with
            | Some v -> v, env // Hope the type matches or check?
            | None ->
                let v = ask path typ
                let env = Map.add path v env
                v, env

        let rec step env k =
            match k with
            | Continuation.Value x -> x
            | Continuation.Query(path, typ, k) ->
                let v, env = lookup path typ
                step env (k v)

        step Map.empty (Eval.evalBenefit benefit)
