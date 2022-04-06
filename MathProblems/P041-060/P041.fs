module P041

open Common

let isNDigitPandigital (n:int64) =
    let digits = List.unfold(fun a -> if a > 0L then Some(a % 10L, a / 10L) else None ) n
    if digits |> List.exists(fun a -> a = 0L || a > int64 digits.Length) then
        false
    else
        digits |>
        List.sort |>
        List.pairwise |>
        List.exists(fun (a,b) -> a = b) |>
        not

let solve =
    seq {7654321..-2..2} |>
    Seq.map int64 |>
    Seq.filter isNDigitPandigital |>
    Seq.filter isPrime |>
    Seq.head