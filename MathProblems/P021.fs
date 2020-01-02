module P021

let sum_prop_div n =
    if n < 4
    then 1
    else
        let sq = double n |> sqrt |> int
        seq {2..sq}
        |> Seq.map(fun i -> if n % i = 0 then i + n / i else 0)
        |> Seq.sum
        |> fun i -> i - (if sq * sq = n then sq else 0)
        |> (+) 1

let solve =
    let max = 9999
    let arr_sums =
        [|0..max|]
        |> Array.map sum_prop_div
    
    seq {1..max}
    |> Seq.map(fun i -> 
        if (arr_sums.[i] > i) && (arr_sums.[i] <= max) &&  (arr_sums.[arr_sums.[i]] = i) 
        then i + arr_sums.[i] 
        else 0)
    |> Seq.sum