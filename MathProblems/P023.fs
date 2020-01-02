module P023

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
    let max = 20161
    
    let abundant_numbers =
        [1..max]
        |> List.filter(fun n -> n < sum_prop_div n)

    let abundant_sums = 
        [for a1 in abundant_numbers do 
            for a2 in abundant_numbers do 
                yield a1 + a2]
        |> Set.ofList
    
    seq {1..max}
    |> Seq.filter(fun n -> abundant_sums |> Set.contains n |> not)
    |> Seq.sum

    
