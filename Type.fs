namespace Solala

module Empty =
    type t = Impossible of t

module Type =
    type EnumType = { name : string; ctors : string list}

    type 't TypeVar = { label: string; mutable parent: 't option }

    and 'var t =
        | Bool
        | Int
        | String
        | Date
        | List of 'var t
        | EnumType of EnumType
        | Var of 'var * 'var t TypeVar

    exception RuntimeTypeError of string

    type vtype = unit t
    type ctype = Empty.t t

    let genTypeVar =
        let mutable i = 0L
        fun () ->
            let j = i
            i <- i + 1L
            Var((), { label = $"typevar-{j}"; parent = None })

    let find t =
        let mutable depth = 0

        let rec find =
            function
            | Var(_, tvar) as t ->
                match tvar.parent with
                    | None -> t
                    | Some parent ->
                        let t = find parent
                        depth <- depth + 1
                        tvar.parent <- Some t
                        t
            | t -> t

        find t, depth

    let rec toString t =
        find t
        |> fst
        |> function
            | Bool -> "boolean"
            | Int -> "number"
            | String -> "string"
            | Date -> "date"
            | List t -> $"list of {toString t}"
            | EnumType t -> t.name
            | Var (_, tvar) -> tvar.label

    let rec union t1 t2 =
        let t1, d1 = find t1
        let t2, d2 = find t2
        match t1, t2 with
        | t1, t2 when t1 = t2 -> t1
        | Var(_, tvar1), Var(_, tvar2) ->
            let p1, d1 = find t1
            let p2, d2 = find t2
            if d1 < d2 then
                tvar2.parent <- Some p1
                p1
            else
                tvar1.parent <- Some p2
                p2
        | Var(_, tvar), t | t, Var(_, tvar) ->
            tvar.parent <- Some t
            t
        | List t1, List t2 -> List (union t1 t2)
        | _ -> failwith $"Type mismatch, got {t1} but expected {t2}"

    let rec concreteType t =
        match find t |> fst with
        | Var _ -> failwith "Could not infer concrete type"
        | Bool -> Bool
        | Int -> Int
        | String -> String
        | Date -> Date
        | EnumType t -> EnumType t
        | List t -> List (concreteType t)
