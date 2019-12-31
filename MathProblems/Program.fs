open System

open P011

[<EntryPoint>]
let main argv =
    printfn "%d" solve
    Console.ReadKey() |> ignore
    0
