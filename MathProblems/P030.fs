module P030

let isSumOfDigitPowers e n =
    let sum = 
        Seq.unfold(fun a -> if a > 0 then Some(a % 10, a / 10) else None ) n 
        |> Seq.map(fun d -> double d ** double e |> int)
        |> Seq.sum
    n = sum

let solve =
    seq {2..999999}
    |> Seq.filter (fun d -> isSumOfDigitPowers 5 d)
    |> Seq.sum
