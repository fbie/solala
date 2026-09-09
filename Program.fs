namespace Solala

module Test =
    open Sugar
    open Fragment

    let f1 =
        fragment
            "Sociallov pp xx"
            "Reimbursement of driving cost"
            (all
                [ assertion "Child is under 18" (child %-> "age" %< n 18)
                  assertion "Child has permanent disability" (child %-> "disability" %-> "permanence" %= s "permanent")
                  judgement "Child benefits from transport" ])

    // lumo.proton.me generated, edited by me:

    // =====================================================================
    // Serviceloven section 100 - kompensationsydelse (2025-niveau)
    // Rule: every assertion and judgement carries its verbatim law fragment
    // as the anchor (first string argument).
    // =====================================================================

    // -- Stk. 1, 1. pkt.: personkreds --
    // "til personer mellem det fyldte 18. år og folkepensionsalderen,
    //  jf. § 1 a i lov om social pension, med varigt nedsat fysisk eller
    //  psykisk funktionsevne"
    let stk1Person =
        all
            [ assertion
                  "personer mellem det fyldte 18. år og
                       folkepensionsalderen"
                  (applicant %-> "age" %> n 17.) // >= 18  <=>  > 17 (integer age)
              assertion
                  "med varigt nedsat fysisk eller psykisk
                       funktionsevne"
                  (applicant %-> "impairment" %-> "duration" %= s "permanent")
              // kind check folded into duration assertion above; see note below
              ]

    // -- Stk. 1, 2. pkt.: expense qualification --
    // "Det er en betingelse, at de kompensationsberettigende udgifter er en
    //  konsekvens af den nedsatte funktionsevne"
    // "nodvendige" has no statutory threshold  =>  judgement point
    let stk1Udgift =
        all
            [ judgement "nødvendige kompensationsberettigende udgifter ved den daglige livsførelse"
              assertion
                  "en konsekvens af den nedsatte funktionsevne"
                  (applicant %-> "extraCosts" %-> "cause" %= s "impairment") ]

    // -- Stk. 1, sidste punktum: blocking exhaustion condition --
    // "ikke kan dækkes efter anden lovgivning eller andre bestemmelser
    //  i denne lov"
    let stk1Udtmning =
        all
            [ assertion
                  "ikke kan dækkes efter anden lovgivning eller andre bestemmelser i denne lov"
                  (exhausted
                      [ "serviceloven § 112" // aids
                        "serviceloven § 85" // practical help / care
                        "sygeforsikring \"Danmark\"" ])
              requires "??" bankStatement
              requires "??" receipt ]

    // -- Stk. 4: exception, "er ikke berettiget ... medmindre ..." --
    let stk4Undtagelse =
        not_ (
            all
                [ assertion
                      "Personer, der modtager pension efter § 14 i lov om ... førtidspension m.v."
                      (applicant %-> "pension" %-> "kind" %= s "foertidspension-per-sec14")
                  not_ (
                      any
                          [ assertion
                                "tillige er bevilget kontant tilskud efter § 95"
                                (applicant %-> "receiving" %-> "sec95" %= s "true")
                            assertion
                                "eller borgerstyret personlig assistance efter § 96"
                                (applicant %-> "receiving" %-> "bpa96" %= s "true") ]
                  ) ]
        ) // NOT ( pension AND NOT (sec95 OR sec96) )
    // expressed with not_/all/any only, as Sugar provides

    // -- Stk. 2: amount thresholds and documentation regimes --
    // "når borgerens sandsynliggjorte kompensationsberettigende udgifter
    //  udgør mindst 6.660 kr. (2025-niveau) pr. år svarende til 555 kr.
    //  (2025-niveau) pr. måned"
    let stk2Taerskel: Predicate = applicant %-> "extraCosts" %-> "yearly" %> n 6659. // >= 6660  <=>  > 6659

    let stk2Standardgren =
        all
            [ requires "sandsynliggjorte kompensationsberettigende udgifter" probable
              assertion "udgør mindst 6.660 kr. (2025-niveau) pr. år" stk2Taerskel ] // 555-1999 kr./month: probable suffices

    let stk2FaktiskGren =
        all
            [ requires "dokumenterer visse typer af kompensationsberettigende udgifter" receipt
              assertion "på over 24.000 kr. (2025-niveau) pr. år" (applicant %-> "extraCosts" %-> "yearly" %> n 24000.) ]

    let stk2Valg = stk2Standardgren %| stk2FaktiskGren

    // -- Assembly: one fragment, one anchor, full condition tree --
    let sec100 =
        fragment
            "Kommunalbestyrelsen skal yde dækning af nødvendige
             kompensationsberettigende udgifter ved den daglige livsførelse
             ... Det er en betingelse, at de kompensationsberettigende
             udgifter er en konsekvens af den nedsatte funktionsevne og ikke
             kan dækkes efter anden lovgivning eller andre bestemmelser i
             denne lov"
            "serviceloven § 100, stk. 1-4 (2025-niveau)"
            (all [ stk1Person; stk1Udgift; stk1Udtmning; stk4Undtagelse; stk2Valg ])
