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
