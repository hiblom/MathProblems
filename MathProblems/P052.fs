module P052

open System

let toSortedString n =
    n |> string  |> Seq.sort |> String.Concat

let multiplesContainSameDigits n =
    let s = toSortedString n
    seq {2..6}
    |> Seq.map (fun i -> i * n)
    |> Seq.map toSortedString
    |> Seq.exists (fun a -> a <> s)
    |> not

let solve =
    seq {1..Int32.MaxValue}
    |> Seq.filter multiplesContainSameDigits
    |> Seq.head