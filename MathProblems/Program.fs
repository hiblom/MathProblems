open System

open P013

[<EntryPoint>]
let main argv =
    printfn "%A" solve
    Console.ReadKey() |> ignore
    0
