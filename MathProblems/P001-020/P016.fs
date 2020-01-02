module P016

let solve =
    2
    |> bigint
    |> fun i -> i ** 1000
    |> string
    |> Seq.map(fun c -> int c - int '0')
    |> Seq.sum
