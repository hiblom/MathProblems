module P050

let generateSieve m =
    let mutable s = [| for _ in 0..m -> true |]
    s.[0] <- false
    s.[1] <- false
    
    let r = m |> double |> sqrt |> int

    for i in 4..2..m do
        s.[i] <- false
    
    for p in 3..2..r do
        if s.[p] then
            for i in 2 * p..p..m do s.[i] <- false
    s

let getNextPrime (start:int) (sieve: bool[]) =
    {start + 1..sieve.Length - 1}
    |> Seq.filter(fun p -> sieve.[p])
    |> Seq.tryHead

let getMostConsec (start:int) (sieve: bool[]) =
    (start, start, 1) |>
    Seq.unfold(fun (prime, sum, count) ->
        match getNextPrime prime sieve with
        | Some(nPrime) ->
            let nSum = sum + nPrime
            let nCount = count + 1
            if nSum >= sieve.Length then
                None
            elif not sieve.[nSum] then
                Some(None, (nPrime, nSum, nCount))
            else
                let nCount = count + 1
                Some(Some(nSum, nCount), (nPrime, nSum, nCount))
        | None -> None)
    |> Seq.choose(fun a -> a)
    |> Seq.tryLast

let solve =
    let max = 999999
    let sieve = generateSieve max

    seq {2..max} 
    |> Seq.filter(fun p -> sieve.[p])
    |> Seq.choose(fun p -> getMostConsec p sieve)
    |> Seq.maxBy snd
    |> fst


