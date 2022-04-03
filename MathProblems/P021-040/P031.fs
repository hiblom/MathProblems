module P031

let COINS = [|1;2;5;10;20;50;100;200|]

let rec countCombos nr rem =
    if nr = 0
    then 1
    else
        seq {0..COINS.[nr]..rem}
        |> Seq.map(fun a -> countCombos (nr - 1) (rem - a))
        |> Seq.sum

let solve =
    countCombos 7 200
    