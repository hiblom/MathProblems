open System

open P008

[<EntryPoint>]
let main argv =
    printfn "%d" solve
    Console.ReadKey() |> ignore
    0
