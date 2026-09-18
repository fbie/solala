namespace Solala

module Continuation =
    type t<'a, 'b> =
        | Value of 'b
        | Query of string list * Type.ctype * ('a -> t<'a, 'b>)

    let value<'a, 'b> (x: 'b) : t<'a, 'b> = Value x

    let query<'a> typ path =
        Query(path, typ, fun (x: 'a) -> Value x)

    let rec bind (f: 'd -> t<'a, 'b>) : t<'a, 'd> -> t<'a, 'b> =
        function
        | Value x -> f x
        | Query(s, typ, k) -> Query(s, typ, fun x -> bind f (k x))

    let inline (>>=) m f = bind f m

    type ContinuationBuilder private () =
        member _.Bind(m, f) = bind f m
        member _.Return(x: 'a) : t<_, 'a> = Value x
        member _.ReturnFrom(x: t<_, _>) : t<_, _> = x
        member _.Zero() = Value Value._false
        static member val Instance = ContinuationBuilder()

    let cont = ContinuationBuilder.Instance
