namespace Solala

module Lang =
    open Ast

    let inline N n = Value.Int n |> Const
    let inline S s = Value.String s |> Const
    let inline DATE d = Value.Date d |> Const
    let TRUE = Const Value._true
    let FALSE = Const Value._false
    let inline LIST xs = List xs
    let inline C s = Ctor s

    let inline OF s = function
        | Ref r ->Ref { r with path = s :: r.path }
        | _ -> failwith "Expected a reference as argument"

    let inline SYMBOL s = Ref { path = [s]; typ = Type.genTypeVar() }

    let inline NOT a = ApplyUnary(Not, a)

    let inline EQUALS a b = ApplyBinary (Equals, a, b)
    let inline AND a b = ApplyBinary (And, a, b)
    let inline OR a b = ApplyBinary (Or, a, b)
    let IS = EQUALS TRUE
    let HAS = IS

    let inline LESS_THAN a b = ApplyBinary(LessThan, a, b)
    let inline LESS_THAN_EQUAL a b =  OR (LESS_THAN a b)  (EQUALS a b)
    let inline GREATER_THAN a b = ApplyBinary(LessThan, b, a)
    let inline GREATER_THAN_EQUAL a b = OR (GREATER_THAN a b) (EQUALS a b)

    let inline BEFORE a b = ApplyBinary(Before, a, b)
    let inline BEFORE_OR_AT a b = OR (BEFORE a b) (EQUALS a b)
    let inline AFTER a b = ApplyBinary(Before, b, a)
    let inline AFTER_OR_AT a b = OR (AFTER a b) (EQUALS a b)

    let IN a b = ApplyBinary (In, a, b)

    let ASSERT description expression = Assertion { description = description; expression = expression }
    let JUDGE description = Judgment { description = description }

    let BENEFIT name description provides requires : Choice<_ Benefit, Type.EnumType> =
        Choice1Of2 { name = name; description = description; provides = provides; requires = requires }

    let ENUM name ctors : Choice<_ Benefit, Type.EnumType> = Choice2Of2 { name = name; ctors = ctors }

module Law =

    open Lang

    module Serviceloven =
        let prog = [
            ENUM "bopæl" ["hjemme"; "andet"]
            ENUM "funktionsnedsættelse_typ" ["fysisk"; "psykisk"; "ukendt"]
            ENUM "varighed" ["varig"; "midlertidligt"]

            BENEFIT
                "pp41"
                "§ 41. Kommunalbestyrelsen skal yde dækning af nødvendige
                merudgifter ved forsørgelse i hjemmet af et barn under
                18 år med betydelig og varigt nedsat fysisk eller psykisk
                funktionsevne eller indgribende kronisk eller langvarig lidelse.
                Det er en betingelse, at merudgifterne er en konsekvens af den
                nedsatte funktionsevne og ikke kan dækkes efter andre bestemmelser
                i denne lov eller anden lovgivning."

                "Dækning af merudgifter"

                [
                    (* Stk. 1 *)
                    ASSERT "et barn under 18 år"(LESS_THAN (OF "alder" (SYMBOL "barnet")) (N 18))
                    ASSERT "forsørgelse i hjemmet" (EQUALS (C "hjemme") (OF "bopæl" (SYMBOL "barnet")))
                    ASSERT "varigt funktionsnedsættelse" (EQUALS (OF "funktionsnedsættelse" (SYMBOL "barnet")) (C "varig"))
                    ASSERT "fysisk eller psykisk" (IN (OF "type" (OF "funktionsnedsættelse" (SYMBOL "barnet")))  (LIST [C "fysisk"; C "psykisk"]))
                    JUDGE "Merudgifterne er en konsekvens af den nedsatte funktionsevne"
                    JUDGE "Merudgifterne kan ikke dækkes efter andre bestemmelser i denne lov eller anden lovgivning"

                    (* Stk. 2. Udmålingen af ydelsen sker på grundlag af de sandsynliggjorte merudgifter for det enkelte barn, f.eks. merudgifter til individuel befordring og fritidsaktiviteter. *)

                    (* Stk. 3. Beløbet til dækning af de nødvendige merudgifter kan ydes, når de skønnede merudgifter udgør mindst 5.207 kr. pr. år (2022-niveau). Ydelsen fastsættes ud fra de skønnede merudgifter pr. måned og afrundes til nærmeste kronebeløb, der er deleligt med 100. *)
                    ASSERT "skønnede merudgifter udgør mindst 5.207 kr. pr. år" (LESS_THAN_EQUAL (N 5207) (OF "beløbet" (SYMBOL "årlige merudgifter")))

                    (* Stk. 4. Hjælpen efter stk. 1 er betinget af, at kommunalbestyrelsens anvisninger med hensyn til pasning m.v. følges. *)

                    (* Stk. 5. Social- og ældreministeren kan i en bekendtgørelse fastsætte nærmere regler om, hvilke udgifter der kan ydes hjælp til, og betingelserne herfor. *)
                ]

            BENEFIT
                "pp42"
                "§ 42. Kommunalbestyrelsen skal yde hjælp til dækning af tabt arbejdsfortjeneste
                til personer, der i hjemmet forsørger et barn under 18 år med betydelig og
                varigt nedsat fysisk eller psykisk funktionsevne eller indgribende kronisk
                eller langvarig lidelse. Ydelsen er betinget af, at det er en nødvendig
                konsekvens af den nedsatte funktionsevne, at barnet eller den unge passes
                i hjemmet, og at det er mest hensigtsmæssigt, at det er moderen eller faderen,
                der passer barnet."

                "Tabt arbejdsfortjeneste"

                [
                    (* Stk. 1 *)
                    ASSERT "barn under 18 år" (LESS_THAN (OF "alder" (SYMBOL "barnet")) (N 18))
                    ASSERT "forsørgelse i hjemmet" (EQUALS (OF "bopæl" (SYMBOL "barnet")) (C "hjemme"))
                    ASSERT "varigt funktionsnedsættelse" (EQUALS (OF "funktionsnedsættelse" (SYMBOL "barnet")) (C "varig"))
                    ASSERT "fysisk eller psykisk" (IN (OF "type" (OF "funktionsnedsættelse" (SYMBOL "barnet"))) (LIST [C "fysisk"; C "psykisk"]))
                    JUDGE  "Betydelig nedsat funktionsevne eller indgribende kronisk/langvarig lidelse"
                    JUDGE  "Det er en nødvendig konsekvens af funktionsnedsættelsen, at barnet passes i hjemmet"
                    JUDGE  "Det er mest hensigtsmæssigt, at forælder varetager pasningen"
                    ASSERT "Ansøger har tabt arbejdsfortjeneste som følge af pasningsbehovet" (HAS (OF "bevilling af tabt arbejdsfortjeneste" (SYMBOL "ansøger")))

                    (* Stk. 2. Udmålingen af ydelsen sker på grundlag af det enkelte barns individuelle behov for pasning og den forsørgendes normale arbejdsindsats. *)

                    (* Stk. 3. Ydelsen beregnes ud fra den indtægt, som forsøgeren før modtagelsen af ydelsen normalt opnåede, dog højst svarende til maksimumbeløbet pr. måned (27.500 kr. 2016-niveau). *)
                    // ASSERT "maksimumbeløbet overholdes"
                    //     (LESS_THAN_EQUAL
                    //         (OF "månedlig ydelse" (SYMBOL "ansøger"))
                    //         (OF "maksimumsbeløb" (SYMBOL "parametre")))

                    (* Stk. 4. For den, der ikke har opnået tilknytning til arbejdsmarkedet, fastsættes ydelsen på baggrund af den indtægt, som ansøgeren efter sine evner ville kunne opnå. *)

                    (* Stk. 5. En supplerende ydelse kan ydes, hvis forsørgeren mister sit arbejde i perioden hvor tabt arbejdsfortjeneste ydes. *)
                ]
            ]
