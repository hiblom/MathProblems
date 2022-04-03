module P037

open Common

let truncateRight (n:int64) =
    if n < 10L then
        None
    else
        Some(n / 10L )

let truncateLeft (n:int64) =
    if n < 10L then
        None
    else
        let b = int64 (10.0 ** truncate (log10 (double n)))
        Some(n % b)

let isTruncatable (truncate:int64->Option<int64>) (n:int64) =
    n |>
    Seq.unfold (fun a -> 
        match truncate a with
            | Some(b) -> Some(b, b)
            | None -> None) |>
    Seq.exists(fun a -> not(isPrime a)) |>
    not

let solve = 
    Seq.initInfinite(fun i -> int64 i) |>
    Seq.filter (fun a -> a > 10L && isPrime a && isTruncatable truncateLeft a && isTruncatable truncateRight a) |>
    Seq.take 11 |>
    Seq.sum

