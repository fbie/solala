namespace Solala

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

    type EnumDef = Type.EnumType

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
        | Ctor of string
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
        { name: string
          description: string
          provides: string
          requires: 't Condition list }

    type 't Program = Choice<'t Benefit, EnumDef> list

    let rec map f =
        function
        | Const v -> Const v
        | Ctor ctor -> Ctor ctor
        | Ref reference -> Ref(f reference)
        | List es -> List(List.map (map f) es)
        | ApplyUnary(op, e) -> ApplyUnary(op, map f e)
        | ApplyBinary(op, e1, e2) -> ApplyBinary(op, map f e1, map f e2)

    let rec typeConst typ = function
        | Value.Bool _ -> Type.union typ Type.Bool
        | Value.Int _ -> Type.union typ Type.Int
        | Value.Date _ -> Type.union typ Type.Date
        | Value.String _ -> Type.union typ Type.String
        | Value.List xs ->
            List.fold typeConst (Type.genTypeVar ()) xs
            |> Type.union typ

    let typeExpression (ctors : Map<string, _>) typ =
        let rec typeExpression typ =
            function
            | Const v ->
                typeConst typ v |> ignore
                Const v

            | List es ->
              match typ with
                | Type.List typ -> List(List.map (typeExpression typ) es)
                | typ ->
                    let elemType = Type.genTypeVar ()
                    let e = List(List.map (typeExpression elemType) es)
                    Type.union typ (Type.List elemType) |> ignore
                    e

            | Ctor ctor ->
                match Map.tryFind ctor ctors with
                    | Some enumType ->
                        Type.union typ enumType |> ignore
                        Ctor ctor
                    | None -> failwith $"Unknown enum constructor: {ctor}"

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
                | And | Or -> t Type.Bool
                | LessThan -> t Type.Int
                | Before -> t Type.Date
                | In ->
                    let tvar = Type.genTypeVar ()
                    ApplyBinary(In, typeExpression tvar e1, typeExpression (Type.List tvar) e2)

        typeExpression typ

    let rec typeBenefit ctors (benefit: Type.vtype Benefit) : Type.ctype Benefit =
        let typeCondition: Type.vtype Condition -> Type.ctype Condition =
            function
            | Judgment { description = d } -> Judgment { description = d } // ?
            | Assertion assertion ->
                let e =
                    assertion.expression
                    |> typeExpression ctors (Type.genTypeVar ())
                    |> map (fun r ->
                        { path = r.path
                          typ = Type.concreteType r.typ })

                Assertion
                    { description = assertion.description
                      expression = e }

        { name = benefit.name
          description = benefit.description
          provides = benefit.provides
          requires = List.map typeCondition benefit.requires }

    let typeEnum (enumDef : EnumDef) : Type.vtype = Type.EnumType enumDef

    let typeProg (prog : Type.vtype Program) : Map<string, Type.ctype Benefit> =
        let enums =
            prog
            |> List.choose (function Choice2Of2 x -> Some x | _ -> None)
            |> List.map (fun e -> let t = typeEnum e in List.map (fun ctor -> ctor, t) e.ctors)
            |> List.concat
            |> Map.ofList
        prog
        |> List.choose (function Choice1Of2 x -> Some x | _ -> None)
        |> List.map (typeBenefit enums)
        |> List.map (fun x -> x.name, x)
        |> Map.ofList
