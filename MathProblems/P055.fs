module P055

open System
open System.Numerics

let isPalindromic (n:bigint) =
    let ns = n |> string
    let rs = ns |> Seq.rev |> String.Concat
    ns = rs

let reverse (n:bigint) =
    n |> string |> Seq.rev |> String.Concat |> BigInteger.Parse

let isLychrel (n:bigint) =
    let count = 
        (n, 0)
        |> Seq.unfold(fun (a, i) ->
            if i = 50 then 
                None
            else
                let r = reverse a
                let na = a + r
                if isPalindromic na then
                    None
                else
                    Some (i + 1, (na, i + 1)))
        |> Seq.tryLast

    match count with
    | Some(c) -> c >= 50
    | None -> false

let solve =
    seq {1..9999}
    |> Seq.map bigint
    |> Seq.filter isLychrel
    |> Seq.length

    
    
    