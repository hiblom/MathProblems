module P004

let is_palindromic x =
    let s = x.ToString().ToCharArray()
    let len = Array.length s
    seq { 0..len / 2 } |> Seq.exists(fun i -> s.[i] <> s.[len - 1 - i]) |> not

let solve =
    seq {
        for i in 100..999 do
            for j in i..999 -> i*j }
    |> Seq.filter is_palindromic
    |> Seq.max
