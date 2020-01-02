module P026

let rec findPeriodRec (n: int) (a: int) (v: int list) =
    let r = a % n
    if r <> 0 then
        match List.tryFindIndex (fun e -> e = a) v with
            |None -> findPeriodRec n (r * 10) (v @ [a])
            |Some(i) -> List.length v - i
    else 0

let findPeriod (n: int) =
    findPeriodRec n 10 []

let solve =
    seq {2..1000}
    |> Seq.map(fun n -> (n, findPeriod n))
    |> Seq.maxBy(fun (n, p) -> p)
    |> fst