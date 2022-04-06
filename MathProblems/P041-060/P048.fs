module P048

let selfPow n =
    (1L, 1L) 
    |> Seq.unfold(fun (index, product) ->
        if index > n then
            None
        else
            let newProduct = (product * n) % 10000000000L
            Some(newProduct, (index + 1L, newProduct))) 
    |> Seq.last


let solve =
    seq {1L..1000L}
    |> Seq.map selfPow
    |> Seq.sum
    |> fun a -> a % 10000000000L