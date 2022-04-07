module P063

open System

//with base numbers less than 10, the exponent will become greater than the number of digits
//with base number 10 the exponent is the number of digits minus 1 (so never equal)
//with base number greater than 10, the exponent will become less than the number of digits
//so: does not work for base numbers of 10 or greater!

let digitCount n = n |> string |> Seq.length

let countBase (b:int) =
    (bigint 1, 0)
    |> Seq.unfold(fun (p, e) ->
        let p1 = p * bigint b
        let e1 = e + 1
        let dc = digitCount p1
        if dc = e1 then
            Some(1, (p1, e1))
        else
            None)
    |> Seq.sum

let solve =
    {1..9}    
    |> Seq.map countBase
    |> Seq.sum