﻿module Common

let rec gcd (a:int64) (b:int64) = 
    if b = 0L
    then a
    else gcd b (a % b)

let lcm (a:int64) (b:int64) =
    a * b / gcd a b

let isPrime (n:int64) =
    if n < 2L then
        false
    elif n = 2L then
        true
    elif n % 2L = 0L then
        false
    else
        let r = double n |> sqrt |> int64
        seq {3L..2L..r} 
        |> Seq.exists(fun i -> n % i = 0L) 
        |> not

let isPandigital (n:int64) (minDigit:int) (maxDigit:int) =
    let digits = List.unfold(fun a -> if a > 0L then Some(a % 10L, a / 10L) else None ) n
    if digits |> List.exists(fun a -> a < int64 minDigit || a > int64 maxDigit) then
        false
    else
        digits |>
        List.sort |>
        List.pairwise |>
        List.exists(fun (a,b) -> a = b) |>
        not


    