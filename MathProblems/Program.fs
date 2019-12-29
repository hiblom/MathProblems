open System

open P003

[<EntryPoint>]
let main argv =
    printfn "%d" solve
    Console.ReadKey() |> ignore
    0
