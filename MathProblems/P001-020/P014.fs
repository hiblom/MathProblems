module P014

let collatz_chain_length n =
    int64 n
    |> Seq.unfold(fun a -> 
        if a = 1L 
        then None 
        else Some(a, if a % 2L = 0L then a / 2L else 3L * a + 1L))
    |> Seq.length

let solve =
    seq {2..999999}
    |> Seq.map(fun i -> (i, collatz_chain_length i))
    |> Seq.maxBy(fun (i, c) -> c)
    |> fst
