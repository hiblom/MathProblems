module P047

open System

let rec countDistinctPrimeFactors n =
    if n < 4 then
        0
    else
        let r = n |> double |> sqrt |> int
        (n, r, 2, 0) |>
        Seq.unfold(fun(num, root, currentFactor, lastFactor) ->
            if currentFactor > root then
                if num > 1 then
                    let res = if num <> lastFactor then 1 else 0
                    Some(res, (1, root, currentFactor, lastFactor))
                else
                    None
            elif num % currentFactor = 0 then
                let res = if currentFactor <> lastFactor then 1 else 0
                let newNum = num / currentFactor
                let newRoot = newNum |> double |> sqrt |> int
                Some(res, (newNum, newRoot, currentFactor, currentFactor))
            else
                let newFactor = if currentFactor = 2 then 3 else currentFactor + 2
                Some(0, (num, root, newFactor, lastFactor))) |>
        Seq.sum

let solve =
    seq {11..Int32.MaxValue} |>
    Seq.map(fun a -> (a, countDistinctPrimeFactors a)) |>
    Seq.windowed 4 |>
    Seq.filter(fun a -> a |> Array.exists(fun a1 -> snd a1 <> 4) |> not ) |>
    Seq.head |>
    Array.head |>
    fst
