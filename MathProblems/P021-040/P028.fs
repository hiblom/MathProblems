module P028

let solve =
    seq { 
        for a in 2..2..1000 do 
        for b in 0..3 do 
        yield a}
    |> Seq.fold (
        fun(n, sum) a -> 
        (n + a, sum + n + a))
        (1, 0)
    |> snd
    |> (+) 1

