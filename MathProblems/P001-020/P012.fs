module P012

open TriangularNumberSequence

let count_divisors n =
    let sq = double n |> sqrt |> int
    seq {2..sq - 1} |> Seq.filter (fun i -> n % i = 0) |> Seq.length |> (*) 2 |> (+) (if n % sq = 0 then 1 else 0)

let solve =
    tri_seq |> Seq.map(fun t -> (t, count_divisors t)) |> Seq.filter(fun (t, d) -> d > 500) |> Seq.head |> fst