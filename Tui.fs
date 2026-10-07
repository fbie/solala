namespace Solala

module Tui =
    open System
    open Ast

    type env = Map<string list, Value.t>

    let refute<'a> : 'a = failwith "Refuted case"

    let parse f (s: string) =
        if String.IsNullOrEmpty s then
            Ok None
        else
            try
                Ok(f s)
            with :? FormatException as e ->
                Error e.Message

    let rec retry handleError f x =
        match f x with
        | Error e ->
            handleError e
            retry handleError f x
        | Ok x -> x

    let query (path: string list) (t: string) () =
        let label = path |> List.rev |> String.concat "s "
        printf $"Indtast {t} for {label} og tryk enter: "
        Console.ReadLine()

    let tryParseWith (tryParse : string -> bool * _) = tryParse >> function
        | true, v -> Some v
        | false, _   -> None

    let runBenefit (env: env) (benefit: Type.ctype Ast.Benefit) =
        let inline (=>>) m f x = Option.map f (m x)

        let rec ask path (typ : Type.ctype) = // Takes not a Ref but path and type separately - lists only ask for their element type!
            let query label k = query path label >> parse k

            let query =
                match typ with
                | Type.Bool ->
                    query "'ja' eller 'nej'" (function "ja" -> Some Value._true | "nej" -> Some Value._false | _ -> None)
                | Type.Int ->
                    query "et heltal" (tryParseWith Int32.TryParse =>> Value.Int)
                | Type.Date ->
                    query "et dato" (tryParseWith DateOnly.TryParse =>> Value.Date)
                | Type.String ->
                    query "text" (fun s -> s.Replace ('\n', char 0) |> Value.String |> Some)
                | Type.List t ->
                    let rec loop vs () =
                        match ask path t with
                        | None -> List.rev vs
                        | Some v -> loop (v :: vs) ()

                    loop [] >> Value.List >> Option.Some >> Ok
                | Type.EnumType t ->
                    let parse s = if List.contains s t.ctors then Some (Value.String s) else None // Enums are just strings at run-time.
                    let ctors = List.map (sprintf "'%s'") t.ctors |> String.concat ", "
                    query $"en af {ctors}" parse

                | Type.Var (_, _) -> refute

            retry (printfn "FEJL: %s") query ()

        let rec lookup (env: env) (r : Type.ctype Ref) =
            match Map.tryFind r.path env with
            | Some v -> v, env // Hope the type matches or check?
            | None ->
                match ask r.path r.typ with
                    | Some v ->
                        let env = Map.add r.path v env
                        v, env
                    | None -> lookup env r

        let rec step env k =
            match k with
            | Continuation.Value x -> x
            | Continuation.Query(r, k) ->
                let v, env = lookup env r
                step env (k v)

        step Map.empty (Eval.evalBenefit benefit)
