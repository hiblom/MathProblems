module P042

open System.IO

let getWordScore s = 
    s |> Seq.sumBy(fun c -> int c - int 'A' + 1)

let isTriangularNumber n =
    let a = 8 * n + 1
    let r = a |> double |> sqrt |> int
    a = r * r

let solve =
    File.ReadAllText("D:\Development\euler-files\p042_words.txt")
    |> fun s -> s.Split [|','|] 
    |> Seq.map(fun s -> s.Trim [|'"'|])
    |> Seq.map getWordScore
    |> Seq.filter isTriangularNumber
    |> Seq.length