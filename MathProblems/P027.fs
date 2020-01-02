module P027

open Common

let quadraticPrimesLength (a: int64) (b:int64) =
    Seq.initInfinite(fun n -> int64 n * int64 n + a * int64 n + b)
    |> Seq.takeWhile is_prime
    |> Seq.length

let solve =
    seq { 2L..1000L }
    |> Seq.filter is_prime
    |> Seq.map(fun b -> seq { for a in -1000L..1000L do yield (a, b) })
    |> Seq.concat
    |> Seq.map(fun (a, b) -> (a, b, quadraticPrimesLength a b))
    |> Seq.maxBy(fun (a, b, n) -> n)
    |> fun (a, b, _) -> a * b