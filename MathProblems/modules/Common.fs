﻿module Common

let rec gcd (a:int64) (b:int64) = 
    if b = 0L
    then a
    else gcd b (a % b)

let lcm (a:int64) (b:int64) =
    a * b / gcd a b

let is_prime (n:int64) =
    if n < 2L then
        false
    elif n = 2L || n = 3L then
        true
    elif n % 2L = 0L then
        false
    else
        let ub = (double n |> sqrt |> int) + 1
        let f = seq {2..ub} |> Seq.takeWhile(fun i -> n % int64 i <> 0L) |> Seq.last
        //let f = seq { 2..sq } |> Seq.takeWhile(fun i -> n % int64 i <> 0L) |> Seq.last
        f = ub
