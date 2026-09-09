module Fragment =
    type Entity = Applicant | Child
    type Const = String of string | Number of float

    type Path = { property : Property; step : string }
    and Property =
        | Entity of Entity
        | Property of Path

    type Val = Const of Const | Property of Property

    type Documentation = MedicalStatement | BankStatement | Receipts | Probable

    type Predicate =
        | Documented  of Documentation
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

    type t = {
        anchor : string;
        benefit : string;
        condition : Condition
    }

module E =
    open Fragment

    let applicant = Entity Applicant
    let child = Entity Child

    let s s = Const (String s)
    let n f = Const (Number f)
    let dot property step = Property { property = property; step = step }

    let medicalStatement = MedicalStatement
    let bankStatement = BankStatement
    let receipt = Receipts
    let probable = Probable


    let documented documentation = Documented documentation
    let equals a b = Equals (a, b)
    let notEquals a b = NotEquals (a, b)
    let lessThan a b = LessThan (a, b)
    let greaterThan a b = GreaterThan (a, b)
    let in_ x xs = In (x, xs)
    let notIn x xs = NotIn xs
    let exhausted xs = Exhausted xs


    let assertion anchor predicate : Condition = Assertion {
        anchor = anchor;
        predicate = Some predicate
    }

    let judgement anchor : Condition = Judgement {
        anchor = anchor;
    }

    let all cs : Condition = And cs
    let and_ c1 c2 : Condition = all [c1; c2]
    let any cs : Condition = Or cs
    let or_ c1 c2 : Condition = any [c1; c2]
    let not_ x : Condition = Not x

    let fragment anchor benefit condition : t = {
        anchor = anchor;
        benefit = benefit;
        condition = condition
    }

open E

let e1 =
    fragment
      "Sociallov pp xx"
      "Reimbursement of driving cost"
      (and_
        (assertion "Child is under 18"  (lessThan (property child "age") (n 180)))
        (assertion "Child has permanent disability" (equals (property child "disability permanence") (s "permanent")))
        (not_ (judgement "Child benefits from transport")))
