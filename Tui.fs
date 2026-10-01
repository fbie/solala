namespace Solala

module Tui =
    open System
    open Ast

    type env = Map<string list, Value.t>

    let refute<'a> : 'a = failwith "Refuted case"

    let run (env: env) (benefit: Type.ctype Ast.Benefit) =
        let parse f (s: string) =
            if String.IsNullOrEmpty s then
                Ok None
            else
                try
                    Ok(Some(f s))
                with :? FormatException as e ->
                    Error e.Message

        let rec retry handleError f x =
            match f x with
            | Error e ->
                handleError e
                retry handleError f x
            | Ok x -> x

        let query (path: string list) (t: string) () =
            let label = String.concat " of " path
            printf $"Please enter {t} for {label} and press enter: "
            Console.ReadLine()

        let rec ask path typ = // Takes not a Ref but path and type separately - lists only ask for their element type!
            let query label k = query path label >> parse k

            let query =
                match typ with
                | Type.Bool -> query "true or false" (bool.Parse >> Value.b)
                | Type.Int -> query "a number" (int >> Value.Int)
                | Type.Date -> query "a date" (DateOnly.Parse >> Value.Date)
                | Type.String -> query "text" (fun s -> s.Replace ('\n', char 0) |> Value.String)
                | Type.List t ->
                    let rec loop vs () =
                        match ask path t with
                        | None -> List.rev vs
                        | Some v -> loop (v :: vs) ()

                    loop [] >> Value.List >> Option.Some >> Ok
                | _ -> refute

            retry (printfn "ERROR: %s") query ()

        let lookup (env: env) (r : Type.ctype Ref) =
            match Map.tryFind r.path env with
            | Some v -> v, env // Hope the type matches or check?
            | None ->
                let v = ask r.path r.typ |> Option.get
                let env = Map.add r.path v env
                v, env

        let rec step env k =
            match k with
            | Continuation.Value x -> x
            | Continuation.Query(r, k) ->
                let v, env = lookup env r
                step env (k v)

        step Map.empty (Eval.evalBenefit benefit)
