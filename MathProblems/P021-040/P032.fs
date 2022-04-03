module P032

let hasUniqueDigits1 (m: int) =
    let digits = m |> string |> Seq.toList
    (List.contains '0' digits |> not) && (digits |> List.length) = (digits |> List.distinct |> List.length)

let hasUniqueDigits3 (m: int) (n: int) (p: int) =
    let digits = (string m + string n + string p) |> Seq.toList
    (List.contains '0' digits |> not) && (digits |> List.length) = (digits |> List.distinct |> List.length)

let hasLength9 (m: int) (n: int) (p: int) =
    let digits = (string m + string n + string p) |> Seq.toList
    List.length digits = 9

let solve =
    seq {1..987}
    |> Seq.filter (fun m -> hasUniqueDigits1 m)
    |> Seq.map(fun m -> seq {for n in (m + 1)..9876 do yield (m,n) })
    |> Seq.concat
    |> Seq.map(fun (m, n) -> (m, n, m * n))
    |> Seq.filter(fun (m, n, p) -> hasLength9 m n p)
    |> Seq.filter(fun (m, n, p) -> hasUniqueDigits3 m n p)
    |> Seq.map(fun (_, _, p) -> p)
    |> Seq.distinct
    |> Seq.sum
