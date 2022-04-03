module P034

let fac =
    Seq.unfold(fun (i, fac) -> if i < 10 then Some (fac, (i + 1, fac * (i + 1))) else None) (0, 1)
    |> Seq.toArray

let isSumOfDigitFactorials n =
    let sum = 
        Seq.unfold(fun a -> if a > 0 then Some(a % 10, a / 10) else None ) n 
        |> Seq.map(fun d -> fac.[d])
        |> Seq.sum
    n = sum

let solve =
    seq {10..9999999}
    |> Seq.filter isSumOfDigitFactorials
    |> Seq.sum    
