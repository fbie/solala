namespace Solala

module Value =
    type t =
        | Bool of bool
        | Int of int
        | String of string
        | Date of System.DateOnly
        | List of t list

    let _true = Bool true
    let _false = Bool false

    let b b = if b then _true else _false

    let isTrue =
        function
        | Bool false
        | List [] -> false
        | _ -> true // False and empty list are false values, everything else is true

    let isFalse = isTrue >> not

    let band a b =
        if isTrue a && isTrue b then _true else _false

    let bor a b =
        if isFalse a && isFalse b then _false else _true

    let bnot a = if isTrue a then _false else _true
