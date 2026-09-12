namespace Solala

module Ast =
    (**
    define symbol child Barnet. -- Already defined, do not query.
    define symbol applicant Ansøger.

    define assertion is_guardian guardian of child equals applicant. -- Comparison of properties, query for nested property.
    define assertion permanent_handicap is_permanent of handicap of child. -- Does this property exist? Query!

    define benefit some_name
      provides Kompensation af kørselsudgifter
      requires
        is_guardian and permanent_handicap


*)
    type Symbol = { name: string; description: string }

    type Of = { reference: Ref; property: string }

    and Ref =
        | Ref of string
        | Of of Of

    type Const =
        | String of string
        | Int of int
        | Date of System.DateTime

    type Predicate =
        | Exists of Ref
        | Not of Predicate
        | And of Predicate * Predicate
        | Or of Predicate * Predicate
        | Equals of Ref * Ref
        | LessThan of Ref * Ref
        | GreaterThan of Ref * Ref // <>, <= and >= are sugar.

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

module Existential =
    type E =
        abstract member Apply: 'a F -> 'a

    and 'a F =
        abstract member Apply: 'x -> 'a

    let e x : E =
        { new E with
            member _.Apply f = f.Apply x }

    let just x : 'a F =
        { new F<'a> with
            member _.Apply _ = x }

    let apply (f: 'a F) (e: E) : 'a = e.Apply f

module Continuation =
    type 'a t =
        | Value of 'a
        | Query of string * 'a t Existential.F

    let value x = Value x
    let query label k = Query(label, k)

    let rec bind (f: 'a -> 'b t) : 'a t -> 'b t =
        function
        | Value x -> f x
        | Query(s, k) ->
            Query(
                s,
                { new Existential.F<'b t> with
                    member _.Apply x = bind f (k.Apply x) }
            )

    let inline (>>=) m f = bind f m

    type ContinuationBuilder private () =
        member _.Bind(m, f) = bind f m
        member _.Return(x: 'a) : 'a t = Value x
        member _.ReturnFrom x = x
        static member val Instance = ContinuationBuilder()

    let cont = ContinuationBuilder.Instance

module Eval =
    open Ast
    open Continuation

    let just x = Existential.just (Continuation.value x)

    let rec evalRef: Ref -> unit Continuation.t =
        function
        | Ref name -> query name (just ())
        | Of { reference = r; property = p } ->
            cont {
                do! evalRef r
                return! query p (just ())
            }

// let rec evalPredicate
