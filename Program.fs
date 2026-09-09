open Fragment
open Sugar

let e1 =
    fragment
        "Sociallov pp xx"
        "Reimbursement of driving cost"
        (all
            [ assertion "Child is under 18" (child %-> "age" %< n 18)
              assertion "Child has permanent disability" (child %-> "disability" %-> "permanence" %= s "permanent")
              judgement "Child benefits from transport" ])
