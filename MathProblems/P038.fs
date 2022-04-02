module P038

open Common

let minProduct = 123456789L
let maxProduct = 987654321L

let paste n m =
    if n = 0L then
        m
    else
        int64 (string n + string m)

let getMaxPandigitalProduct n =
    let sq =
        (0L, 0L) |>
        Seq.unfold(fun (i, s) ->
            if i = 9L then 
                None 
            else
                let nextI = i + 1L
                let nextS = paste s (nextI * n)
                Some((nextI, nextS),(nextI, nextS))) |>
        Seq.map snd |>
        Seq.filter(fun a -> a >= minProduct) |>
        Seq.takeWhile(fun a -> a <= maxProduct) |>
        Seq.filter(fun a -> isPandigital a 1 9)

    if Seq.length sq = 0 then
        None
    else
        let res = Seq.last sq
        Some(res)

let solve =
    {1L..50000L} |>
    Seq.choose getMaxPandigitalProduct |>
    Seq.max
