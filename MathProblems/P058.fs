module P058

open System
open Common

let solve =
    seq { for a in 2..2..Int32.MaxValue do for b in 1..4 do yield a } 
    |> Seq.scan (fun (primes, total, num, _) side ->
        if isPrime (int64 num) then
            (primes + 1, total + 1, num + side, side)
        else
            (primes, total + 1, num + side, side)) (0, 0, 1, 2)
    |> Seq.takeWhile (fun (primes, total, _, _) -> primes = 0 || total = 0 || primes * 10 > total)
    |> Seq.last
    |> fun (_, _, _, side) -> side + 1
    