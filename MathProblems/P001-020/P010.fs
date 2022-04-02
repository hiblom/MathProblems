module P010

open Common

let solve =
    seq {2L..2000000L}
    |> Seq.filter isPrime
    |> Seq.sum