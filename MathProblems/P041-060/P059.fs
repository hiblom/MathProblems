module P059

open System
open System.IO

let solve =
    let cipher = "exp"
    File.ReadAllText("D:\Development\euler-files\p059_cipher.txt")
        |> fun s -> s.Split [|','|]
        |> Seq.map (fun s -> int s)
        |> Seq.mapi (fun i n -> int cipher.[i % 3] ^^^ n)
        |> Seq.sum
