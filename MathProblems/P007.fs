module P007

open System
open Common

let solve =
    Seq.initInfinite(fun i -> int64 i)
    |> Seq.filter is_prime
    |> Seq.take 10001
    |> Seq.last