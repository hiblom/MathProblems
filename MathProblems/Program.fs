open System

open P017

[<EntryPoint>]
let main argv =
    printfn "%A" solve
    Console.ReadKey() |> ignore
    0
