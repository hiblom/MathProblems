module P062

open System
open System.Collections.Generic

let mutable cubeMap = new Dictionary<string, int*int64>()

let sort n = n |> string |> Seq.sort |> String.Concat

let solve =
    {1..Int32.MaxValue}
    |> Seq.map int64
    |> Seq.choose(fun n -> 
        let c = n * n * n
        let key = sort c
        
        if not (cubeMap.ContainsKey key) then do
            cubeMap.[key] <- (1, c)
        else do
            cubeMap.[key] <- (fst cubeMap.[key] + 1, snd cubeMap.[key])

        if fst cubeMap.[key] = 5 then
            Some(snd cubeMap.[key])
        else
            None)
    |> Seq.head
