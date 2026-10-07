namespace Solala

module Main =
    open System

    [<EntryPoint>]
    let main _ =
        let env = Map.empty
        let benefits = Ast.typeProg Law.Serviceloven.prog
        let rec loop () : int =
            printfn "Vælg en paragraf du vil søge:"
            Map.iter (fun p _ -> printfn $" - {p}") benefits
            let p = (Console.ReadLine ()).Trim ()
            if not (String.IsNullOrEmpty p) then
                match Map.tryFind p benefits with
                | Some benefit ->
                    let eligibility = Tui.runBenefit Map.empty benefit
                    System.Console.WriteLine eligibility
                | None ->
                    printfn $"FEJL: {p} kunne ikke findes"
            loop ()
        loop ()
