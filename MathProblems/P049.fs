module P049

open System
open Common

let rec permute (s:string) (r:string) =
    if String.length r = 0 then
        [s]
    else
        let c = r.[0] |> string
        let newR = r.[1..]
        [0..String.length s]
        |> List.map(fun i -> 
            let newS = s.[0..i - 1] + c + s.[i..]
            permute newS newR)
        |> List.concat

let getPermutations s =
    permute "" s

let isNotDecreasing (n:int) =
    n
    |> string
    |> Seq.toList
    |> Seq.pairwise
    |> Seq.exists(fun (a, b)-> a > b)
    |> not

let solve =
    let mutable alreadyProcessed = Set.empty<int>

    seq {1000..9999}
    //|> Seq.filter isNotDecreasing
    |> Seq.filter (int64>>isPrime)
    |> Seq.choose(fun a ->
        if alreadyProcessed.Contains a then
            None
        else
            alreadyProcessed <- alreadyProcessed.Add a
            let perms = getPermutations (string a) |> List.sort |> List.distinct
            let permPrimes = perms |> List.map int64 |> List.filter isPrime
            for p in permPrimes do
                alreadyProcessed <- alreadyProcessed.Add (int p)

            if List.length permPrimes < 3 then 
                None
            else
                //must be equi-distant
                let distPermPrimes =
                    [for i in 0..List.length permPrimes - 2 do
                        for j in i..List.length permPrimes - 1 do
                            yield (i, j) ]
                    |> List.map(fun (i, j) -> (permPrimes.Item i, permPrimes.Item j))
                    |> List.filter(fun (p1, p2) -> p2 - p1 = 3330L)

                (*
                let distPermPrimes = 
                    seq {
                        for i in 0..List.length permPrimes - 2 do
                            for j in i..List.length permPrimes - 1 do
                                yield (i, j) }
                        |> Seq.map(fun (i, j) -> (permPrimes.Item i, permPrimes.Item j))
                        |> Seq.filter(fun (p1, p2) -> p2 - p1 = 3330L)
                *)

                if Seq.length distPermPrimes < 2 then
                    None
                else
                    Some(distPermPrimes))