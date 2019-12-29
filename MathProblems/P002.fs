module P002

open FibonacciSequence

let solve =
    fib_seq 
        |> Seq.takeWhile(fun i -> i <= 4000000)
        |> Seq.filter(fun i -> i % 2 = 0)
        |> Seq.sum