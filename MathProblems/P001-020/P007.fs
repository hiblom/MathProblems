module P007

open System
open Common

let solve =
    Seq.initInfinite(fun i -> int64 i)
    |> Seq.filter isPrime
    |> Seq.take 10001
    |> Seq.last