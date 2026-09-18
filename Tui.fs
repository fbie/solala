namespace Solala

module Tui =
    open System
    open Ast

    type env = Map<string list, Value.t>

    let run (env: env) (benefit: Type.ctype Ast.Benefit) =
        let parse f (s: string) =
            if String.IsNullOrEmpty s then
                Ok None
            else
                try
                    Ok(Some(f s))
                with :? FormatException as e ->
                    Error e.Message

        let rec retry f x =
            match f x with
            | Error(e: string) ->
                printf $"Error: {e}"
                retry f x
            | Ok x -> x

        let query (path: string list) (t: string) () =
            let label = String.concat "of" path
            printf $"Please enter {t} for {label} and press enter:"
            Console.ReadLine()

        let rec ask path typ=
            let query label k = query path label >> parse k

            let query =
                match typ with
                | Type.Bool -> query "true or false" (bool.Parse >> Value.b)
                | Type.Int -> query "a number" (int >> Value.Int)
                | Type.Date -> query "a date" (DateOnly.Parse >> Value.Date)
                | Type.String -> query "text" Value.String
                | Type.List t ->
                    let rec loop vs () =
                        match ask path t with
                        | None -> List.rev vs
                        | Some v -> loop (v :: vs) ()

                    loop [] >> Value.List >> Option.Some >> Ok
                | _ -> failwith "Cannot happen"

            retry query ()

        let lookup (env: env) path typ =
            match Map.tryFind path env with
            | Some v -> v, env // Hope the type matches or check?
            | None ->
                let v = ask path typ |> Option.get
                let env = Map.add path v env
                v, env

        let rec step env k =
            match k with
            | Continuation.Value x -> x
            | Continuation.Query(path, typ, k) ->
                let v, env = lookup env path typ
                step env (k v)

        step Map.empty (Eval.evalBenefit benefit)
