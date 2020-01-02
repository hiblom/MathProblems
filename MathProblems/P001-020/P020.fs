module P020

open System

let solve =
    seq {1..100}
    |> Seq.fold (fun a e -> a * bigint e) (bigint 1)
    |> string
    |> Seq.sumBy (Char.GetNumericValue >> int)