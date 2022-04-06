module P046

open Common

let isSumOfPrimeAndTwiceSquare n =
    let r = n / 2 |> double |> sqrt |> int
    seq {1..r} |>
    Seq.choose(fun a -> 
        let p = n - 2 * a * a
        if isPrime (int64 p) then Some a else None) |>
    Seq.tryHead

let solve =
    Seq.initInfinite (fun a -> a) |>
    Seq.skip 10 |>
    Seq.filter(fun a -> a % 2 = 1) |>
    Seq.filter(fun a -> not (isPrime(int64 a))) |>
    Seq.choose(fun a -> 
        match isSumOfPrimeAndTwiceSquare a with
        | Some(a) -> None
        | None -> Some(a)) |>
    Seq.head
