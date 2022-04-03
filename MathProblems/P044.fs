module P044

let pent n = n * (3 * n - 1) / 2

let isPentagonal n =
    let a = 24 * n + 1
    let r = a |> double |> sqrt |> int
    a = r * r && r % 6 = 5

let solve =
    2 |>
    Seq.unfold(fun a -> Some(seq { for b in a - 1..-1..1 do yield (a, b) }, a + 1 )) |>
    Seq.concat |>
    Seq.choose(fun (a, b) -> 
        let pentA = pent a
        let pentB = pent b

        if isPentagonal (pentA - pentB) && isPentagonal (pentA + pentB) 
        then 
            Some(pentA - pentB) 
        else 
            None) |>
    Seq.head