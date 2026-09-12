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

    let sec100 =
        fragment
            "Kommunalbestyrelsen skal yde dækning af nødvendige
             kompensationsberettigende udgifter ved den daglige livsførelse
             ... Det er en betingelse, at de kompensationsberettigende
             udgifter er en konsekvens af den nedsatte funktionsevne og ikke
             kan dækkes efter anden lovgivning eller andre bestemmelser i
             denne lov"
            "serviceloven § 100, stk. 1-4 (2025-niveau)"
            (all
                [
                  // -- Stk. 1, 1. pkt.: personkreds --
                  // "til personer mellem det fyldte 18. år og folkepensionsalderen,
                  //  jf. § 1 a i lov om social pension, med varigt nedsat fysisk eller
                  //  psykisk funktionsevne"
                  all
                      [ assertion
                            "personer mellem det fyldte 18. år og folkepensionsalderen"
                            ((applicant %-> "age").isGreaterThan 17.) // >= 18  <=>  > 17 (integer age)
                        assertion
                            "med varigt nedsat fysisk eller psykisk funktionsevne"
                            ((applicant %-> "impairment" %-> "duration").is "permanent") ]
                  // -- Stk. 1, 2. pkt.: expense qualification --
                  // "Det er en betingelse, at de kompensationsberettigende udgifter er en
                  //  konsekvens af den nedsatte funktionsevne"
                  all
                      [ judgement "nødvendige kompensationsberettigende udgifter ved den daglige livsførelse"
                        assertion
                            "en konsekvens af den nedsatte funktionsevne"
                            ((applicant %-> "extraCosts" %-> "cause").is "impairment") ]
                  // -- Stk. 1, sidste punktum: blocking exhaustion condition --
                  assertion
                      "ikke kan dækkes efter anden lovgivning eller andre bestemmelser i denne lov"
                      (exhausted
                          [ "serviceloven § 112" // helpers
                            "serviceloven § 85" // practical help / care
                            "sygeforsikring \"Danmark\"" ])
                  // -- Stk. 4: exception, "er ikke berettiget ... medmindre ..." --
                  not_ (
                      all
                          [ assertion
                                "Personer, der modtager pension efter § 14 i lov om ... førtidspension m.v."
                                ((applicant %-> "pension" %-> "kind").is "foertidspension-per-sec14")
                            not_ (
                                any
                                    [ assertion
                                          "tillige er bevilget kontant tilskud efter § 95"
                                          ((applicant %-> "receiving" %-> "sec95").is "true")
                                      assertion
                                          "eller borgerstyret personlig assistance efter § 96"
                                          ((applicant %-> "receiving" %-> "bpa96").is "true") ]
                            ) ]
                  )
                  any
                      [ all
                            [ requires "sandsynliggjorte kompensationsberettigende udgifter" probable
                              assertion
                                  "udgør mindst 6.660 kr. (2025-niveau) pr. år"
                                  ((applicant %-> "extraCosts" %-> "yearly").isGreaterThan 6659.) ]
                        all
                            [ requires "dokumenterer visse typer af kompensationsberettigende udgifter" receipt
                              assertion
                                  "på over 24.000 kr. (2025-niveau) pr. år"
                                  ((applicant %-> "extraCosts" %-> "yearly").isGreaterThan 24000.) ] ] ])
