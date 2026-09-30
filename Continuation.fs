namespace Solala

module Continuation =
    type t<'a, 'b, 'c> =
        | Value of 'b
        | Query of 'c * ('a -> t<'a, 'b, 'c>)

    let value<'a, 'b, 'c> (x: 'b) : t<'a, 'b, 'c> = Value x
    let query<'a, 'c> (q : 'c)    : t<'a, 'a, 'c> = Query(q, Value)

    let rec map f =
        function
        | Value x -> Value (f x)
        | Query (q, k) -> Query (q, k >> map f)

    let rec bind (f: 'd -> t<'a, 'b, 'q>) : t<'a, 'd, 'q> -> t<'a, 'b, 'q> =
        function
        | Value x -> f x
        | Query(q, k) -> Query(q, k >> bind f)

    let (>>=) m f = bind f m

    type ContinuationBuilder private () =
        member _.Bind(m, f) = bind f m
        member _.Return(x: 'a) : t<_, 'a, _> = Value x
        member _.ReturnFrom(x: t<_, _, _>) : t<_, _, _> = x
        member _.Zero() = Value Value._false
        static member val Instance = ContinuationBuilder()

    let cont = ContinuationBuilder.Instance
