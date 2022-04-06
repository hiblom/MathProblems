module P057

let digitCount n =
    n |> string |> Seq.length

let solve =
    (bigint 1, bigint 1)
    |> Seq.unfold(fun (nom, den) ->
        let nDen = nom + den
        let nNom = den + nDen
        let valid = if digitCount nNom > digitCount nDen then 1 else 0
        Some (valid, (nNom, nDen)))
    |> Seq.take 1000
    |> Seq.sum