module P033

open Common

let isCurious num den =
    let f = double num / double den

    ((num % 10) = (den % 10)) && (f = double (num / 10) / double (den / 10)) ||
    ((num % 10) = (den / 10)) && (f = double (num / 10) / double (den % 10)) ||
    ((num / 10) = (den % 10)) && (f = double (num % 10) / double (den / 10)) ||
    ((num / 10) = (den / 10)) && (f = double (num % 10) / double (den % 10))

let solve =
    seq { for num in 11..99 do for den in (num + 1)..99 do yield (num, den) }
    |> Seq.filter (fun (num, den) -> num % 10 <> 0  && den % 10 <> 0)
    |> Seq.filter (fun (num, den) -> isCurious num den)
    |> Seq.reduce (fun a e -> (fst a * fst e, snd a * snd e))
    |> fun (num, den) -> den / int (gcd (int64 num) (int64 den))
    