module P020

open System

let solve =
    seq {1..100}
    |> Seq.map(bigint)
    |> Seq.reduce((*))
    |> string
    |> Seq.sumBy(fun c -> int c - int '0')