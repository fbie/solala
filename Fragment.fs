module Fragment

type Entity = Applicant | Child
type Const = String of string | Number of float

type Property = Entity of Entity | Step of Property * string
type Documentation = MedicalStatement | BankStatement | Receipts | Probable

type Val =
    | Const of Const
    | Property of Property
        with
            static member op_Equality(a, b) = Equals (a, b)
            static member op_Inequality(a, b) = NotEquals (a, b)
            static member op_LessThan(a, b) = LessThan (a, b)
            static member op_GreaterThan(a, b) = GreaterThan (a, b)
            static member op_Dot(a, b) = Property (Step (a, b))

and Predicate =
    | Requires    of Documentation
    | Equals      of Val * Val
    | NotEquals   of Val * Val
    | LessThan    of Val * Val
    | GreaterThan of Val * Val
    | In          of Val * Val list
    | NotIn       of Val * Val list
    | Exhausted   of string list // Refrences to other laws - should be a proper reference to a definition!w

type Assertion = { anchor : string; predicate : Predicate option } // Factual condition.
type Judgement = { anchor : string; } // Judgement by the municipality.

type Condition =
    | Assertion of Assertion
    | Judgement of Judgement
    | And of Condition list
    | Or of Condition list
    | Not of Condition

    with
        static member op_BooleanAnd (a, b) = And [a; b]
        static member op_BooleanOr (a, b) = Or [a; b]
        static member (~%)  a = Not a

type t = {
    anchor : string;
    benefit : string;
    condition : Condition
}
