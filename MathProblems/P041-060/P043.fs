module P043

let div = [|0; 2; 3; 5; 7; 11; 13; 17|]

let rec countNumbers (currentDigits:List<int>) (remainingDigits:List<int>) =
    let start = currentDigits.Length - 3
    
    let valid = 
        if start < 1 then 
            true
        else
            let subNum = 100 * currentDigits.Item start + 10 * currentDigits.Item (start + 1) + currentDigits.Item (start + 2)
            subNum % div.[start] = 0

    if not valid then
        0L
    elif remainingDigits.Length = 0 then
        currentDigits |> List.map string |> List.reduce (+) |> int64
    else
        seq {0..remainingDigits.Length - 1} |>
        Seq.map(fun a -> countNumbers (currentDigits @ [remainingDigits.Item a]) (remainingDigits.[..a - 1] @ remainingDigits.[a + 1..])) |>
        Seq.sum

let solve =
    countNumbers [] [0..9] 