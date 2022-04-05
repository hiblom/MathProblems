module P053

let over (n:bigint) (m:bigint) =
    let nom = seq {m + bigint 1..n} |> Seq.fold (*) (bigint 1)
    let den = seq {bigint 1..n - m} |> Seq.fold (*) (bigint 1)
    nom/den

let solve =
    seq { bigint 1..bigint 100 }
    |> Seq.map(fun n -> 
        seq {bigint 1..n - bigint 1} |> Seq.filter(fun m -> over n m > bigint 1000000) |> Seq.length)
    |> Seq.sum