module E =
    open Fragment

    let applicant = Entity Applicant
    let child = Entity Child

    let s s : Val = Const (String s)
    let n f : Val = Const (Number f)
    let step property step : Val = Property (Step (property, step))

    let medicalStatement = MedicalStatement
    let bankStatement = BankStatement
    let receipt = Receipts
    let probable = Probable

    let requires documentation = Requires documentation
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
        (all [
            (assertion "Child is under 18"  ((step child "age") < (n 180)));
            (assertion "Child has permanent disability" ((step (step child "disability") "permanence") = (s "permanent")));
            (% (judgement "Child benefits from transport"))])
