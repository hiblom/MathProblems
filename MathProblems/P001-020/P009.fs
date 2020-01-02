module P009

let solve =
    seq { 
        for a in 1..333 do
            for b in a + 1 .. 666 do
                yield (a, b)}
    |> Seq.map(fun(a, b) -> (a, b, 1000 - a - b))
    |> Seq.filter(fun(a, b, c) -> a * a + b * b = c * c)
    |> Seq.map(fun(a, b, c) -> a * b * c)
    |> Seq.last