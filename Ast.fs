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

    type Ref =
        { mutable typ: Type.t
          path: string list } // Infer types!

    type Variant =
        | Const of Value.t
        | Ref of Ref

    type Predicate =
        | Exists of Ref // t -> bool
        | Not of Predicate // bool -> bool
        | And of Predicate * Predicate // t * t -> bool
        | Or of Predicate * Predicate // t * t -> bool
        | Equals of Variant * Variant // t * t -> bool
        | LessThan of Variant * Variant // t * t -> bool
        | GreaterThan of Variant * Variant // t * t -> bool
        | In of Variant * Variant // t * t list -> bool

    type Judgment = { description: string }

    type Assertion =
        { assertion: Predicate
          description: string }

    type Condition =
        | Judgment of Judgment
        | Assertion of Assertion

    type Benefit =
        { description: string
          provides: string
          requires: Condition list }

module Continuation =
    type t<'a, 'b> =
        | Value of 'a
        | Query of string list * Type.t * ('b -> t<'a, 'b>)

    let value x = Value x

    let query<'a> typ path =
        Query(path, typ, fun (x: 'a) -> Value x)

    let rec bind (f: 'c -> t<'a, 'b>) : t<'c, 'b> -> t<'a, 'b> =
        function
        | Value x -> f x
        | Query(s, typ, k) -> Query(s, typ, fun x -> bind f (k x))

    let inline (>>=) m f = bind f m

    type ContinuationBuilder private () =
        member _.Bind(m, f) = bind f m
        member _.Return(x: 'a) : t<'a, _> = Value x
        member _.ReturnFrom x = x
        static member val Instance = ContinuationBuilder()

    let cont = ContinuationBuilder.Instance

module Eval =
    open Ast

    let cont = Continuation.cont
    let query = Continuation.query
    let (>>=) = Continuation.(>>=)

    let evalRef { typ = typ; path = path } : Continuation.t<Value.t, _> = query (Type.concreteType typ) path

    let evalVariant =
        function
        | Const value -> cont { return value }
        | Ref reference -> evalRef reference

    let rec evalPredicate: Predicate -> _ =
        function
        | Exists reference ->
            evalRef reference
            >>= function
                | Value.Bool b -> cont { return b }
                | _ -> failwith "Unexpected: reference not of type boolean"

        | Not p ->
            cont {
                let! b = evalPredicate p
                return not b
            }

        | And(a, b) ->
            cont {
                let! a = evalPredicate a
                if a then return! evalPredicate b else return false // Only evaluate b if a holds.
            }

        | Or(a, b) ->
            cont {
                let! a = evalPredicate a
                if a then return true else return! evalPredicate b
            }

        | In(x, xs) ->
            cont {
                let! x = evalVariant x
                let! xs = evalVariant xs

                match xs with
                | Value.List xs -> return List.contains x xs
                | _ -> return false //! raise (Type.RuntimeTypeError "Expected a list")
            }

        | Equals(a, b) ->
            cont {
                let! a = evalVariant a
                let! b = evalVariant b
                return a = b
            }

        | LessThan(a, b) ->
            cont {
                let! a = evalVariant a
                let! b = evalVariant b
                return a < b
            }

        | GreaterThan(a, b) ->
            cont {
                let! a = evalVariant a
                let! b = evalVariant b
                return a > b
            }

    type Result =
        | Success
        | Failure of string
        | Discretional of string

    let evalCondition =
        function
        | Assertion { description = d; assertion = a } ->
            cont {
                let! b = evalPredicate a
                return if b then Success else Failure d
            }
        | Judgment { description = description } -> cont { return Discretional description }

    type EligibilityConditions =
        { discretionals: string list
          failures: string list }

    let evalBenefit { requires = rqs } : Continuation.t<EligibilityConditions, _>=
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
