open System

open P021

[<EntryPoint>]
let main argv =
    printfn "%A" solve
    Console.ReadKey() |> ignore
    0
