module P051

open Common

let increase (n:List<int>) =
    if n |> List.exists(fun a -> a <> 9) |> not then
        None
    else
        seq {n.Length - 1..-1..-1}
        |> Seq.fold(fun (acc, c) i -> 
            if i >= 0 then
                let d = (n.Item i + c) % 10
                let nc = if (n.Item i + c) >= 10 then 1 else 0
                ([d] @ acc, nc)
            elif i = -1 && c = 1 then
                ([1] @ acc, 0)
            else
                (acc, 0)) (List.empty<int>, 1)
        |> fun (a, _) -> Some(a)

let rec takePositions (p:List<int>) (n:int) =
    if n >= p.Length then
        [p]
    else
        [0..p.Length - 1]
        |> List.map(fun i -> p.[..i - 1] @ p.[i + 1..])
        |> List.map(fun np -> takePositions np n)
        |> List.concat

let componseNumber (n:List<int>) (p:List<int>) (a:int) =
    let length = n.Length + p.Length
    seq {0..length - 1} 
    |> Seq.fold(fun (acc, npos) i ->
        if p |> List.contains i then 
            (10 * acc + a, npos)
        else
            (10 * acc + n.[npos], npos + 1)) (0, 0)
    |> fun (a, _) -> a


let countNumberPatternPrimes (n:List<int>) (p:List<int>) =
    seq {0..9}
    |> Seq.map(fun a -> componseNumber n p a)
    |> Seq.filter (int64>>isPrime)
    |> Seq.length

let getFirstPatternPrime (n:List<int>) (p:List<int>) =
    seq {0..9}
    |> Seq.map(fun a -> componseNumber n p a)
    |> Seq.filter (int64>>isPrime)
    |> Seq.head

let getPatternMaxPrimes (l:int) (p:List<int>) =
    let s = 
        if p.Item 0 = 0 then
            [for _ in 1..l -> 0]
        else
            [1] @ [for _ in 2..l -> 0]
    let found = 
        Some(s)
        |> Seq.unfold(fun a -> 
            match a with
            | Some(aa) ->
                let primes = countNumberPatternPrimes aa p
                Some((aa, primes), (increase aa))
            | None ->
                None)
        |> Seq.maxBy snd

    if snd found < 8 then
        None
    else
        Some (getFirstPatternPrime (fst found) p)

let getMaxForPatterns (normalDigits:int) (patternDigits:int) =
    //generate valid patterns
    let validPositions = [for i in 0..normalDigits + patternDigits - 2 -> i]
    
    takePositions validPositions patternDigits
    |> List.choose(fun p -> 
       match getPatternMaxPrimes normalDigits p with
       | Some(prime) -> Some (p, prime)
       | None -> None)
    |> List.head

let solve =
    //takePositions [2;3;4;5;6] 3
    //componseNumber [5;3;2] [1;3;5] 6 //should be 563626
    //countNumberPatternPrimes [5;6;3] [2;3]
    //increase [9;9;9] // [1;2;4]
    //getPatternMaxPrimes 3 [0;2;4]
    getMaxForPatterns 3 3