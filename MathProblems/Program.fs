﻿open System

open P040

[<EntryPoint>]
let main argv =
    let start = DateTime.Now
    printfn "%A" solve
    printfn "elapsed time: %d ms" (int (DateTime.Now - start).TotalMilliseconds)
    Console.ReadKey() |> ignore
    0
