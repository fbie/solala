namespace Solala

module Main =
    [<EntryPoint>]
    let main _ =
        let eligibility = Tui.run Map.empty (Ast.typeBenefit Law.Serviceloven.pp41)
        System.Console.WriteLine eligibility
        0
