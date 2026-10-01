namespace Solala

module Lang =
    open Ast

    let inline N n = Value.Int n |> Const
    let inline S s = Value.String s |> Const
    let inline DATE d = Value.Date d |> Const
    let TRUE = Const Value._true
    let FALSE = Const Value._false
    let inline LIST xs = List xs

    let inline OF s = function
        | Ref r ->Ref { r with path = s :: r.path }
        | _ -> failwith "Expected a reference as argument"

    let inline SYMBOL s = Ref { path = [s]; typ = Type.genTypeVar() }

    let inline NOT a = ApplyUnary(Not, a)

    let inline EQUALS a b = ApplyBinary (Equals, a, b)
    let inline AND a b = ApplyBinary (And, a, b)
    let inline OR a b = ApplyBinary (Or, a, b)
    let IS = EQUALS TRUE

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

    let BENEFIT description provides requires = { description = description; provides = provides; requires = requires }

module Law =

    open Lang

    module Serviceloven =
        let pp41 =
            BENEFIT
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
                    ASSERT "forsørgelse i hjemmet" (EQUALS (S "hjemme") (OF "bopæl" (SYMBOL "barnet")))
                    ASSERT "varigt funktionsnedsættelse" (IS (OF "varig" (OF "funktionsnedsættelse" (SYMBOL "barnet"))))
                    ASSERT "fysisk eller psykisk" (IN (OF "funktionsnedsættelse" (SYMBOL "barnet")) (LIST [S "fysisk"; S "psykisk"]))
                    JUDGE "Merudgifterne er en konsekvens af den nedsatte funktionsevne"
                    JUDGE "Merudgifterne kan ikke dækkes efter andre bestemmelser i denne lov eller anden lovgivning"

                    (* Stk. 2. Udmålingen af ydelsen sker på grundlag af de sandsynliggjorte merudgifter for det enkelte barn, f.eks. merudgifter til individuel befordring og fritidsaktiviteter. *)

                    (* Stk. 3. Beløbet til dækning af de nødvendige merudgifter kan ydes, når de skønnede merudgifter udgør mindst 5.207 kr. pr. år (2022-niveau). Ydelsen fastsættes ud fra de skønnede merudgifter pr. måned og afrundes til nærmeste kronebeløb, der er deleligt med 100. *)
                    ASSERT "skønnede merudgifter udgør mindst 5.207 kr. pr. år" (LESS_THAN_EQUAL (N 5207) (OF "beløbet" (SYMBOL "årlige merudgifter")))

                    (* Stk. 4. Hjælpen efter stk. 1 er betinget af, at kommunalbestyrelsens anvisninger med hensyn til pasning m.v. følges. *)

                    (* Stk. 5. Social- og ældreministeren kan i en bekendtgørelse fastsætte nærmere regler om, hvilke udgifter der kan ydes hjælp til, og betingelserne herfor. *)
                ]
