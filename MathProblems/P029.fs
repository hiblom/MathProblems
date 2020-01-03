module P029

let solve =
    seq {
        for a in 2..100 do
            for b in 2..100 do
                yield (a, b)}
    |> Seq.map(fun (a, b) -> (bigint a) ** b)
    |> Set.ofSeq
    |> Set.count