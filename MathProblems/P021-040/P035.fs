module P035

open System
open Common

let rotate (s:string) =
    let sa = Seq.toArray s
    (sa |> Array.tail |> String) + ([|Array.head sa|]|> String)

let getRotations n =
    let ns = string n
    ns |>
    Seq.unfold (fun (s:string) -> 
        let rs = rotate s 
        if rs = ns  then None else Some(int64 rs, rs))

let allRotationsPrime n =
    getRotations n |>
    Seq.exists(fun x -> isPrime x |> not) |>
    not

let solve =
    {2L..999999L} |>
    Seq.filter isPrime |>
    Seq.filter allRotationsPrime |>
    Seq.length