module P025

let FibSeq = (bigint 0, bigint 1) |> Seq.unfold (fun (a, b) -> Some (a + b, (b, a + b))) 

let solve =
    let lb = (bigint 10) ** 999

    FibSeq
    |> Seq.mapi(fun i n -> (i, n)) 
    |> Seq.filter(fun (i, n) -> n >= lb)
    |> Seq.head
    |> fun (i, n) -> i + 2 //index has offset of -2