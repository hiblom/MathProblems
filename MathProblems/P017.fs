module P017

let LOWS=[|
    "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten";
    "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
let TENS = [|
    "zero"; "ten"; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]
let HUNDRED = "hundred"
let AND = "and";
let THOUSAND = "thousand";

let count_thousands (n, c) =
    let d = n / 1000
    if d > 0 then
        let count = LOWS.[d] + THOUSAND |> Seq.length
        n % 1000, c + count
    else
        n, c

let count_hundreds (n, c) =
    let d = n / 100
    if d > 0 then
        let count = LOWS.[d] + HUNDRED + (if (n % 100) <> 0 then AND else "") |> Seq.length
        n % 100, c + count
    else
        n, c

let count_tens (n, c) =
    let d = n / 10
    if d > 1 then
        let count = TENS.[d] |> Seq.length
        n % 10, c + count
    else
        n, c

let count_lows (n, c) =
    if n > 0 then
        let count = LOWS.[n] |> Seq.length
        0, c + count
    else
        0, c

let solve =
    seq {1..1000}
    |> Seq.map(fun n -> (n, 0) |> count_thousands |> count_hundreds |> count_tens |> count_lows)
    |> Seq.sumBy(fun (n, c) -> c)
