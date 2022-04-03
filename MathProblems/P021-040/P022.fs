module P022

open System.IO

let CalcNameScore s = s |> Seq.sumBy(fun c -> int c - int 'A' + 1)

let solve =
    File.ReadAllText("D:\Development\euler-files\p022_names.txt")
    |> fun s -> s.Split [|','|] 
    |> Array.map(fun s -> s.Trim [|'"'|])
    |> Array.sort
    |> Array.mapi(fun i s -> (i + 1) * CalcNameScore s)
    |> Array.sum
    
    