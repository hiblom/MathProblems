module P056

let digitalSum (n:bigint) =
    n |> string |> Seq.map(fun c -> int c - int '0') |> Seq.sum

let solve =
    seq { for a in 1..100 do for b in 1..100 -> (a, b)}
    |> Seq.map (fun (a,b) -> bigint a ** b)
    |> Seq.map (fun a -> digitalSum a )
    |> Seq.max