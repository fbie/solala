namespace Solala

module Fragment =

    /// An entity
    type Entity =
        | Applicant
        | Child

    type Const =
        | String of string
        | Number of float

    type Documentation =
        | MedicalStatement
        | BankStatement
        | Receipts
        | Probable

    type Property =
        | Entity of Entity
        | Step of Variant * string

    and Variant =
        | Const of Const
        | Property of Property

        member x.is y = Equals(x, y)
        member x.is(y: string) = x.is (Const(String y))
        member x.is(y: float) = x.is (Const(Number y))

        member x.isNot y = NotEquals(x, y)
        member x.isNot(y: string) = x.isNot (Const(String y))
        member x.isNot(y: float) = x.isNot (Const(Number y))

        member x.isLessThan y = LessThan(x, y)
        member x.isLessThan(y: float) = x.isLessThan (Const(Number y))

        member x.isGreaterThan y = GreaterThan(x, y)
        member x.isGreaterThan(y: float) = x.isGreaterThan (Const(Number y))


    and Predicate =
        | Requires of Documentation
        | Equals of Variant * Variant
        | NotEquals of Variant * Variant
        | LessThan of Variant * Variant
        | GreaterThan of Variant * Variant
        | In of Variant * Variant list
        | NotIn of Variant * Variant list
        | Exhausted of string list // Refrences to other laws - should be a proper reference to a definition!w

    type Assertion =
        { anchor: string; predicate: Predicate } // Factual condition.

    type Judgement = { anchor: string } // Judgement by the municipality.

    type Condition =
        | Assertion of Assertion
        | Judgement of Judgement
        | And of Condition list
        | Or of Condition list
        | Not of Condition

    type t =
        { anchor: string
          benefit: string
          condition: Condition }

    module OptionM =
        let bind f =
            function
            | None -> None
            | Some x -> f x

        let map f = bind (fun x -> Some(f x))

        type OptionBuilder private () =
            member _.Bind(m, f) = bind f m
            member _.Return x = Some x

            static member val Instance = OptionBuilder()

        let opt = OptionBuilder.Instance

    module ListX =
        let rec foldUntil f p s xs =
            if p s then
                s
            else
                match xs with
                | [] -> s
                | x :: xs -> foldUntil f p (f x s) xs

    module Prefix =
        open OptionM

        type 'a t =
            | Branch of Map<string, 'a t>
            | Leaf of 'a

        exception CannotStepIntoLeaf of string
        exception StepAlreadyExists of string

        let step t k : 'a t -> 'a t option =
            match t with
            | Leaf _ -> raise (CannotStepIntoLeaf k)
            | Branch m -> Map.tryFind k m

        let find (ks: string list) (t: 'a t) : 'a option =
            opt {
                let! x = ListX.foldUntil (fun (Some t) k -> step t k) Option.isNone ks

                match x with
                | Leaf x -> return x
                | _ -> None
            }

        let putStep k v =
            function
            | Leaf _ -> raise (CannotStepIntoLeaf k)
            | Branch m ->
                if Map.containsKey k m then
                    raise (StepAlreadyExists k)
                else
                    Branch(Map.add k v m)

    module Env =

        type t =
            { applicant: Variant Prefix.t
              child: Variant Prefix.t
              documentation: Set<Documentation> }

        let hasDocumentation (env: t) required = Set.contains required env.documentation

        let get (env: t) property Variant option =
            function
            | Child -> Map.tryFind env.child property
            | Applicant -> Map.tryFind env.applicant property

    module Eval =
        type continuation =
            { env: Env.t
              hole: Choice<Documentation, Property>
              frontier: Condition }

        type 'a t =
            | Eligible
            | NotEligible of Condition
            | Ask of Env.t * 'a

// let rec evalVariant variant (env : Env.t) =
//     match variant with
//         | Const _ as c -> c
//         |

//  let rec evalPredicate pred (env: Env.t) =
//      match pred with
//      | Requires doc when Env.hasDocumentation env doc -> Eligible
//      | Equals (a, b) ->
