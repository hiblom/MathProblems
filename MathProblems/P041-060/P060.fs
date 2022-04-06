module P060

open System
open System.Collections.Generic
open Common

let primesList = new List<int>()
let wanted = 4 // actually 5

let concatenate n m =
    string n + string m |> int

let isPrimePair n m =
    concatenate n m |> int64 |> isPrime && concatenate m n |> int64 |> isPrime

let rec findCombination (leftPrimes:list<int>) (chosenPrimes:list<int>) (depth:int) : list<int> option =
    if List.length leftPrimes = 0 then
        None
    else
        [0..leftPrimes.Length - 1]
        |> List.choose(fun i -> 
            if chosenPrimes.Length = 0 || chosenPrimes |> List.exists(fun cp -> not (isPrimePair cp leftPrimes.[i])) |> not then
                if depth = wanted then
                    Some(chosenPrimes @ [leftPrimes.[i]])
                else
                    findCombination leftPrimes.[i + 1..] (chosenPrimes @ [leftPrimes.[i]]) (depth + 1) 
            else
                None)
        |> List.tryHead

let choosePairedPrimes e =
    let pairedPrimes = 
        primesList 
            |> Seq.filter (fun p -> isPrimePair p e) 
            |> Seq.toList

    primesList.Add e

    if List.length pairedPrimes < wanted then 
        None
    else
        //find a combination that works among themselves as well
        match findCombination pairedPrimes [] 1 with
            | Some(foundPrimes) -> Some(foundPrimes @ [e])
            | None -> None

let solve =
    seq {3..2..Int32.MaxValue} 
    |> Seq.filter (int64>>isPrime)
    |> Seq.choose choosePairedPrimes
    |> Seq.head
    |> List.sum
        
    



    
        


