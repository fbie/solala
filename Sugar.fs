namespace Solala

module Sugar =

    open Fragment

    let applicant = Property(Entity Applicant)
    let child = Property(Entity Child)

    let s s : Variant = Const(String s)
    let n f : Variant = Const(Number f)

    let (%->) a b = Property(Step(a, b))

    let (%=) a b = Equals(a, b)
    let (%<>) a b = NotEquals(a, b)
    let (%<) a b = LessThan(a, b)
    let (%>) a b = GreaterThan(a, b)

    let medicalStatement = MedicalStatement
    let bankStatement = BankStatement
    let receipt = Receipts
    let probable = Probable

    let requires anchor documentation =
        Assertion
            { anchor = anchor
              predicate = Requires documentation }

    let (%&) a b = And [ a; b ]
    let (%|) a b = Or [ a; b ]
    let not_ a = Not a
    let in_ x xs = In(x, xs)
    let notIn x xs = NotIn(x, xs)
    let exhausted xs = Exhausted xs

    let assertion anchor predicate : Condition =
        Assertion
            { anchor = anchor
              predicate = predicate }

    let judgement anchor : Condition = Judgement { anchor = anchor }

    let all cs : Condition = And cs
    let any cs : Condition = Or cs

    let fragment anchor benefit condition : t =
        { anchor = anchor
          benefit = benefit
          condition = condition }
