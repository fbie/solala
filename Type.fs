namespace Solala

module Empty =
    type t = Impossible of t

module Type =
    type 't TypeVar = { label: string; mutable parent: 't }

    and 'var t =
        | Bool
        | Int
        | String
        | Date
        | List of 'var t
        | Var of 'var * 'var t TypeVar

    exception RuntimeTypeError of string

    type vtype = unit t
    type ctype = Empty.t t

    let genTypeVar =
        let mutable i = 0L

        fun () ->
            let j = i
            i <- i + 1L

            let rec tvar = // Is its own parent.
                Var(
                    (),
                    { label = $"typevar-{j}"
                      parent = tvar }
                )

            tvar

    let find t =
        let mutable depth = 0

        let rec find =
            function
            | Var(_, tvar) ->
                let t = find tvar.parent
                depth <- depth + 1
                tvar.parent <- t
                t
            | t -> t

        find t, depth

    let union t1 t2 =
        match t1, t2 with
        | t1, t2 when t1 = t2 -> t1
        | Var(_, tvar1), Var(_, tvar2) ->
            let p1, d1 = find tvar1.parent
            let p2, d2 = find tvar2.parent

            if d1 < d2 then
                tvar2.parent <- p1
                p1
            else
                tvar1.parent <- p2
                p2
        | Var(_, tvar), t
        | t, Var(_, tvar) ->
            tvar.parent <- t
            t
        | _ -> failwith "Type mismatch!"

    let rec concreteType: vtype -> ctype =
        function
        | Var(_, { parent = t }) as tvar ->
            if obj.Equals(t, tvar) then
                failwith "Could not infer concrete type"

            let t, _ = find t
            concreteType t
        | Bool -> Bool
        | Int -> Int
        | String -> String
        | Date -> Date
        | List t -> List(concreteType t)
