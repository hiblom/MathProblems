﻿module FibonacciSequence

let fib_seq = (0,1) |> Seq.unfold (fun(a, b) -> Some(a + b, (b, a + b))) 